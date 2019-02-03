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
open Primitives
open FPublisher.Git
open System.IO
open FPublisher.FakeHelper
open Fake.IO.Globbing.Operators

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


        member x.NextReleaseNotes =
            x.ReleaseNotes
            |> ReleaseNotes.updateWithSemVerInfo x.NextVersion
            |> ReleaseNotes.updateDateToToday    



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
    type Msg =
        | ForkerMsg of Forker.Msg
        | EnsureGitChangesAllPushedAndInDefaultBranch
        | WriteReleaseNotesToNextVersionAndPushToRemoteRepository
        | Pack
        | PublishAndDraftAll

    type TargetState =
        { ForkerTargetState: Forker.TargetState
          EnsureGitChangesAllPushedAndInDefaultBranch: SimpleState
          WriteReleaseNotesToNextVersionAndPushToRemoteRepository: SimpleState
          Pack: SimpleState
          PublishAndDraftAll: SimpleState }

    [<RequireQualifiedAccess>]
    module TargetState =
        let init = 
            { ForkerTargetState = Forker.TargetState.init
              EnsureGitChangesAllPushedAndInDefaultBranch = SimpleState.Init
              WriteReleaseNotesToNextVersionAndPushToRemoteRepository = SimpleState.Init
              Pack = SimpleState.Init
              PublishAndDraftAll = SimpleState.Init }


    type Role =
        { NugetPacker: NugetPacker
          TargetPackDir: string
          Forker: Forker.Role
          NugetPublisher: NugetPublisher
          TargetState: TargetState
          Workspace: Workspace
          VersionController: Task<VersionController> }
    with 
        member x.Solution = x.Forker.Solution

        member x.GitHubData = x.VersionController.Result.GitHubData

        interface IRole<TargetState>

    let create (config: Config) =
        let workspace = config.Workspace         

        let forker = Forker.create workspace
        { NugetPacker = config.NugetPacker
          TargetPackDir =
            Path.GetTempPath() </> Path.GetRandomFileName()
            |> Directory.ensureReturn
          NugetPublisher = { ApiEnvironmentName = config.EnvironmentConfig.NugetApiKey }                
          TargetState = TargetState.init
          Workspace = workspace
          VersionController = Config.fetchVersionController config
          Forker = forker }

    let (!^) (forkerMsg: Forker.Msg) = Msg.ForkerMsg forkerMsg

    let run =
        Role.update (function 
            | Msg.ForkerMsg forkerMsg -> 
                let roleAction = Forker.roleAction forkerMsg
                { PreviousMsgs = roleAction.PreviousMsgs |> List.map (!^)
                  Action = 
                    fun role ->
                        roleAction.Action role.Forker }

            | Msg.EnsureGitChangesAllPushedAndInDefaultBranch ->
                { PreviousMsgs = []
                  Action = 
                    fun role -> 
                        let githubData = role.VersionController.Result.GitHubData
                        match githubData.IsInDefaultBranch with 
                        | true ->
                            match (Workspace.repoState role.Workspace) with 
                            | RepoState.Changed -> failwith "Please push all changes to git server before you draft new a release"
                            | RepoState.None -> 
                                failwith "Please push all changes to git server before you draft new a release"
                        | false ->
                            failwithf "Please checkout %s to default branch %s first" githubData.BranchName githubData.DefaultBranch }

            | Msg.WriteReleaseNotesToNextVersionAndPushToRemoteRepository ->
                { PreviousMsgs = 
                    [ Msg.EnsureGitChangesAllPushedAndInDefaultBranch; !^ Forker.Msg.Test ]
                  Action = 
                    fun role -> 
                        let githubData = role.VersionController.Result.GitHubData
                        match githubData.IsInDefaultBranch with 
                        | true ->
                            match (Workspace.repoState role.Workspace) with 
                            | RepoState.Changed -> failwith "Please push all changes to git server before you draft new a release"
                            | RepoState.None -> 
                                failwith "Please push all changes to git server before you draft new a release"
                        | false ->
                            failwithf "Please checkout %s to default branch %s first" githubData.BranchName githubData.DefaultBranch }    
            | Msg.Pack ->
                { PreviousMsgs = [ !^ Forker.Msg.Test ]
                  Action = 
                    fun role -> 
                        let packer = role.NugetPacker
                        let versionController = role.VersionController.Result

                        NugetPacker.pack 
                            role.Solution.Path 
                            role.Workspace 
                            role.GitHubData 
                            role.TargetPackDir 
                            versionController.NextReleaseNotes
                            packer }

            | Msg.PublishAndDraftAll ->
                { PreviousMsgs = [ !^ Forker.Msg.Test ; Msg.Pack ; Msg.WriteReleaseNotesToNextVersionAndPushToRemoteRepository ]
                  Action = 
                    fun role -> 
                        let versionController = role.VersionController.Result

                        let newPackages = 
                            !! (role.TargetPackDir + "./*.nupkg")
                            |> List.ofSeq 

                        [ GitHubData.draftAndPublishWithNewRelease versionController.NextReleaseNotes versionController.GitHubData
                          NugetPublisher.publish newPackages role.NugetPublisher ]
                        |> Async.Parallel
                        |> Async.RunSynchronously
                        |> ignore } 
        )               
                                                                                   
            