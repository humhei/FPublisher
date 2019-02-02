namespace FPublisher.Roles
open FPublisher.Nuget
open Fake.Core
open FPublisher
open FPublisher.GitHub
open FSharp.Control.Tasks.V2.ContextInsensitive
open FPublisher.FakeHelper.Build
open Fake.IO.FileSystemOperators
open FPublisher.Utils
open System.Threading.Tasks
open FPublisher.Types
open Microsoft.Build.Logging.StructuredLogger
open Primitives

[<RequireQualifiedAccess>]
module Collaborator =

    type VersionController =
        { Workspace: Workspace
          VersionFromNugetServer: SemVerInfo option
          ReleaseNotes: ReleaseNotes.ReleaseNotes
          GitHubData: GitHubData }

    with 
        member x.VersionFromReleaseNotes = x.ReleaseNotes.SemVer
   
        member x.ReleaseNotesFile = x.Workspace.WorkingDir </> "RELEASE_NOTES.md"

        member x.WorkingDir = x.Workspace.WorkingDir

        member x.CurrentVersion = 
            [x.VersionFromNugetServer; Some x.VersionFromReleaseNotes]
            |> List.choose id
            |> List.max         

        member x.NextVersion =
            let currentVersion = x.CurrentVersion
            let releaseNotes = x.ReleaseNotes
            let versionFromNugetServer = x.VersionFromNugetServer
            let versionFromReleaseNotes = x.VersionFromReleaseNotes
            
            let nextVersion = 
                    if releaseNotes.Notes.IsEmpty || releaseNotes.Date <> None 
                    then failwith "Please write release notes of new version first"

                    match versionFromNugetServer with 
                    | None ->
                        match versionFromReleaseNotes.PreRelease with
                        | Some _ -> SemVerInfo.nextBetaVersion versionFromReleaseNotes
                        | None -> versionFromReleaseNotes
                        
                    | Some versionFromNugetServer ->
                        if versionFromReleaseNotes < versionFromNugetServer 
                        then SemVerInfo.nextBetaVersion versionFromNugetServer                       
                        else 
                            match versionFromReleaseNotes.PreRelease with 
                            | Some _ -> SemVerInfo.nextBetaVersion versionFromReleaseNotes
                            | None -> versionFromReleaseNotes   

            if nextVersion >= currentVersion then nextVersion      
            else failwithf "next version %s is smaller than current version %s" nextVersion.AsString currentVersion.AsString        



    type Config =
        { NugetPacker: NugetPacker
          EnvironmentConfig: EnvironmentConfig
          Workspace: Workspace }

    with 
        member x.WorkingDir = x.Workspace.WorkingDir


    [<RequireQualifiedAccess>]
    module Config =
        let fetchVersionController (config: Config) = 
            let task = 
                task {
                    Logger.infots "Begin fetch versionController"
                    Logger.infots "Begin fetch github data"
                    let! githubData = GitHubData.fetch config.Workspace config.EnvironmentConfig
                    Logger.infots "End fetch github data"        

                    let workspace = Workspace config.WorkingDir 
                    
                    let releaseNotes = 
                        let releaseNotesFile = workspace.WorkingDir </> "RELEASE_NOTES.md"
                        ReleaseNotes.loadTbd releaseNotesFile

                    Logger.infots "Begin fetch version from nuget server"
                    let! versionFromNugetServer = Workspace.versionFromNugetServer workspace   
                    Logger.infots "End fetch version from nuget server"
                    Logger.infots "End fetch versionController"

                    return 
                        { GitHubData = githubData 
                          Workspace = workspace
                          ReleaseNotes = releaseNotes                
                          VersionFromNugetServer = versionFromNugetServer }
                }
            task.Start()       
            task     


    [<RequireQualifiedAccess>]
    type Target =
        | ForkerTarget of Forker.Target
        | EnsureGitChangesAllPushedAndInDefaultBranchBeforeRelease
        | WriteReleaseNotesToNextVersionAndPushToRemoteRepositoryWhenRelease
        | Pack
        | PublishAndDraftAll


    type Role =
        { NugetPacker: NugetPacker
          Forker: Forker.Role
          NugetPublisher: NugetPublisher
          Status: Target
          VersionController: Task<VersionController> }
    with 
        member x.ChildRole = x.Forker
        member x.NickStatus = x.Status

    let create (config: Config) =

        let workspace = config.Workspace         

        let forker = Forker.create workspace

        { NugetPacker = config.NugetPacker
          NugetPublisher = { ApiEnvironmentName = config.EnvironmentConfig.NugetApiKey }                
          Status = Target.ForkerTarget forker.Status
          VersionController = Config.fetchVersionController config
          Forker = forker }

          


    let run (msg: Target) (role: Role) =
        Primitives.run msg role (function 
            | Target.ForkerTarget forkerStatus ->
                let forker = Forker.run forkerStatus role.Forker
                if UnionCase.isLast forker.Status then 
                    { role with
                        Status = Target.EnsureGitChangesAllPushedAndInDefaultBranchBeforeRelease
                        Forker = forker }
                else 
                    { role with
                        Status = Target.ForkerTarget forker.Status
                        Forker = forker }                         

        )               
                                                                                   
            