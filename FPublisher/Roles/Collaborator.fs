namespace FPublisher.Roles
open Octokit
open FPublisher.Nuget
open Fake.Core
open FPublisher
open FPublisher.GitHub
open FSharp.Control.Tasks.V2.ContextInsensitive
open FPublisher.FakeHelper.Build
open Fake.IO.FileSystemOperators
open System.Threading.Tasks
open Primitives
open FPublisher.Git
open System.IO
open FPublisher.FakeHelper
open Fake.Tools.Git
open Fake.IO
open Fake.Api

type EnvironmentConfig =
    { NugetApiKey: string
      GitHubToken: string
      GitHubReleaseUser: string }

[<RequireQualifiedAccess>]
type PublishTarget = 
    | Build
    | Release

[<RequireQualifiedAccess>]
module Collaborator =

    type GitHubData =
        { Forker: Forker.GitHubData
          ReleaseUserName: string
          BranchName: string
          ReleaseUserToken: string
          CommitHashRemote: string
          CommitHashLocal: string }
    with 

        member x.Repository = x.Forker.Repository

        member x.DefaultBranch = x.Repository.DefaultBranch

        member x.IsInDefaultBranch = x.DefaultBranch = x.BranchName

        member x.Owner = x.Repository.Owner

        member x.LoginName = x.Owner.Login

        member x.RepoName = x.Repository.Name 

        member x.IsLogin = x.LoginName = x.ReleaseUserName

    [<RequireQualifiedAccess>]              
    module GitHubData =

        let draftAndPublishWithNewRelease (releaseNotes: ReleaseNotes.ReleaseNotes) (gitHubData: GitHubData) =
            GitHub.createClientWithToken gitHubData.ReleaseUserToken
            |> GitHubClient.draftAndPublishWithNewRelease gitHubData.ReleaseUserName gitHubData.RepoName releaseNotes

        let fetch forkerGitHubData workspace (environmentConfig: EnvironmentConfig) = task {
            let branchName = Workspace.branchName workspace
            return 
                { Forker = forkerGitHubData
                  ReleaseUserName = Environment.environVarOrFail environmentConfig.GitHubReleaseUser
                  BranchName = branchName
                  ReleaseUserToken = Environment.environVarOrFail environmentConfig.GitHubToken
                  CommitHashLocal = Workspace.commitHashLocal branchName workspace
                  CommitHashRemote = Workspace.commitHashRemote branchName workspace }
        }

    type VersionController =
        { Forker: Forker.VersionController
          VersionFromLocalNugetServer: SemVerInfo option
          GitHubData: GitHubData }

    with   

        member x.Workspace = x.Forker.Workspace

        member x.LastReleaseNotes = x.Forker.LastReleaseNotes

        member x.VersionFromOfficalNugetServer = x.Forker.VersionFromOfficalNugetServer

        member x.TbdReleaseNotes = x.Forker.TbdReleaseNotes

        member x.VersionFromLastReleaseNotes = x.Forker.VersionFromLastReleaseNotes

        member x.VersionFromTbdReleaseNotes = x.Forker.VersionFromTbdReleaseNotes
   
        member x.ReleaseNotesFile = x.Workspace.WorkingDir </> "RELEASE_NOTES.md"

        member x.WorkingDir = x.Workspace.WorkingDir      



    [<RequireQualifiedAccess>]
    module VersionController =

        let currentVersion (versionController: VersionController) = 
            Forker.VersionController.currentVersion versionController.VersionFromLocalNugetServer versionController.Forker

        let nextVersion publishTarget (versionController: VersionController) =
            
            let currentVersion = currentVersion versionController
            let tbdReleaseNotes = versionController.TbdReleaseNotes
            let versionFromOfficalNugetServer = versionController.VersionFromOfficalNugetServer
            let versionFromTbdReleaseNotes = versionController.VersionFromTbdReleaseNotes
            
            let nextVersion = 
                match publishTarget with 
                | PublishTarget.Release ->
                    
                    if tbdReleaseNotes.Notes.IsEmpty || tbdReleaseNotes.Date <> None 
                    then failwith "Please write release notes of new version first"

                    match versionFromOfficalNugetServer with 
                    | None ->
                        match versionFromTbdReleaseNotes.PreRelease with
                        | Some _ -> SemVerInfo.nextBetaVersion versionFromTbdReleaseNotes
                        | None -> versionFromTbdReleaseNotes
                        
                    | Some versionFromNugetServer ->
                        if versionFromTbdReleaseNotes < versionFromNugetServer 
                        then SemVerInfo.nextBetaVersion versionFromNugetServer                       
                        else 
                            match versionFromTbdReleaseNotes.PreRelease with 
                            | Some _ -> SemVerInfo.nextBetaVersion versionFromTbdReleaseNotes
                            | None -> versionFromTbdReleaseNotes  

                | PublishTarget.Build -> SemVerInfo.nextBuildVersion currentVersion

            if nextVersion >= currentVersion then nextVersion      
            else failwithf "next version %s is smaller than current version %s" nextVersion.AsString currentVersion.AsString  

        let nextReleaseNotes publishTarget (versionController: VersionController) =
            versionController.TbdReleaseNotes
            |> ReleaseNotes.updateWithSemVerInfo (nextVersion publishTarget versionController)
            |> ReleaseNotes.updateDateToToday   

        let internal writeReleaseNotesToNextVersionAndPushToRemoteRepository publishTarget (versionController: VersionController) =

            let nextVersion = nextVersion publishTarget versionController

            let releaseNotesFile = versionController.ReleaseNotesFile

            let releaseNotesWithNextVersion = ReleaseNotes.updateWithSemVerInfo nextVersion versionController.TbdReleaseNotes

            ReleaseNotes.writeToNext versionController.ReleaseNotesFile releaseNotesWithNextVersion

            Workspace.git (sprintf "add %s" releaseNotesFile) versionController.Workspace |> ignore

            Commit.exec versionController.WorkingDir (sprintf "Bump version to %s" nextVersion.AsString)

            Branches.push versionController.WorkingDir  

    type Config =
        { NugetPacker: NugetPacker
          EnvironmentConfig: EnvironmentConfig
          WorkingDir: string
          LoggerLevel: Logger.Level
          LocalNugetServer: LocalNugetServer option }

    with 
        member x.Workspace = Workspace x.WorkingDir
        static member DefaultValue =
            { NugetPacker = 
                { Authors = NugetAuthors.GithubLoginName
                  GenerateDocumentationFile = false 
                  SourceLinkCreate = false
                  PackageIconUrl = None
                  BuildingPackOptions = id }
              EnvironmentConfig = 
                { NugetApiKey = "NugetApiKey"
                  GitHubToken = "github_token"
                  GitHubReleaseUser = "github_release_user"}
              WorkingDir = Directory.GetCurrentDirectory()
              LoggerLevel = Logger.Level.Minimal
              LocalNugetServer = None }

    [<RequireQualifiedAccess>]
    module Config =
        let fetchVersionController forkerVersionController solution (config: Config) = 
            let task = 
                task {
                    let workspace = Workspace config.WorkingDir 
                    let! forkerVersionController = forkerVersionController

                    let versionFromLocalNugetServer = 
                        match config.LocalNugetServer with
                        | Some localNugetServer -> 
                            
                            Forker.VersionController.versionFromLocalNugetServer solution localNugetServer forkerVersionController
                            |> Async.RunSynchronously
                        | None -> None

                    let! githubData = GitHubData.fetch forkerVersionController.GitHubData workspace config.EnvironmentConfig

                    return 
                        { GitHubData = githubData 
                          Forker = forkerVersionController               
                          VersionFromLocalNugetServer = versionFromLocalNugetServer }
                }
            task     


    [<RequireQualifiedAccess>]
    type Msg =
        | ForkerMsg of Forker.Msg
        | EnsureGitChangesAllPushedAndInDefaultBranch
        | DraftNewRelease

    type TargetState =
        { ForkerTargetState: Forker.TargetState
          EnsureGitChangesAllPushedAndInDefaultBranch: BoxedState
          DraftNewRelease: BoxedState }

    [<RequireQualifiedAccess>]
    module TargetState =
        let init = 
            { ForkerTargetState = Forker.TargetState.init
              EnsureGitChangesAllPushedAndInDefaultBranch = State.Init
              DraftNewRelease = State.Init }


    type Role =
        { Forker: Forker.Role
          OfficalNugetServer: OfficalNugetServer
          TargetState: TargetState
          LocalNugetServer: LocalNugetServer option
          Permission: Lazy<Permission>
          VersionController: Task<VersionController> }
    with 

        member x.NugetPacker = x.Forker.NugetPacker

        member x.Workspace = x.Forker.Workspace

        member x.Solution = x.Forker.Solution

        member x.GitHubData = x.VersionController.Result.GitHubData

        interface IRole<TargetState> 

    let create (buildingConfig: Config -> Config) =
        let config = buildingConfig Config.DefaultValue
        logger <- Logger.create(config.LoggerLevel)
        let workspace = config.Workspace         

        let forker = Forker.create config.EnvironmentConfig.GitHubToken config.NugetPacker workspace

        let versionController = Config.fetchVersionController forker.VersionController forker.Solution config
        { OfficalNugetServer = { ApiEnvironmentName = config.EnvironmentConfig.NugetApiKey }                
          TargetState = TargetState.init
          VersionController = versionController
          Forker = forker
          LocalNugetServer = config.LocalNugetServer
          Permission =
            lazy 
                { GitHub = 
                    PermissionLevel.Allow 
                  Nuget = PermissionLevel.Deny}
                 }

    let (!^) (forkerMsg: Forker.Msg) = Msg.ForkerMsg forkerMsg
    let (!^^) (nonGitMsg: NonGit.Msg) = !^ (Forker.upcastMsg nonGitMsg)

    let internal roleAction role = function 
        | Msg.ForkerMsg forkerMsg -> 
            let roleAction = Forker.roleAction role.Forker forkerMsg
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
                        | RepoState.None -> none
                    | false ->
                        failwithf "Please checkout %s to default branch %s first" githubData.BranchName githubData.DefaultBranch }

        | Msg.DraftNewRelease ->
            { PreviousMsgs = [Msg.EnsureGitChangesAllPushedAndInDefaultBranch] 
              Action = 
                fun role ->
                    VersionController.writeReleaseNotesToNextVersionAndPushToRemoteRepository publishTarget versionController
                    none }
        //| Msg.Pack publishTarget ->
        //    let nextReleaseNotes =
        //        let versionController = role.VersionController.Result
        //        VersionController.nextReleaseNotes publishTarget versionController
        //    { PreviousMsgs = [ !^ (Forker.Msg.Pack nextReleaseNotes) ]
        //      Action = 
        //        fun role -> 
        //            role.Forker.TargetState.Pack |> State.getResult
        //    }

        //| Msg.PublishAndDraftAll publishTarget ->
        //    match publishTarget with 
        //    | PublishTarget.Release ->
        //        { PreviousMsgs = [ !^^ NonGit.Msg.Test ; Msg.Pack publishTarget ]
        //          Action = 
        //            fun role -> 
        //                let versionController = role.VersionController.Result

        //                let newPackages = role.TargetState.Pack |> State.getResult |> unbox

        //                VersionController.writeReleaseNotesToNextVersionAndPushToRemoteRepository publishTarget versionController

        //                [ yield GitHubData.draftAndPublishWithNewRelease (VersionController.nextReleaseNotes publishTarget versionController) versionController.GitHubData
        //                  yield OfficalNugetServer.publish newPackages role.OfficalNugetServer
        //                  match role.LocalNugetServer with 
        //                  | Some localNugetServer -> yield LocalNugetServer.publish newPackages localNugetServer
        //                  | None -> yield async {()} ]
        //                |> Async.Parallel
        //                |> Async.RunSynchronously
        //                |> ignore
        //                |> box 
        //        } 

        //    | PublishTarget.Build ->
        //        match role.LocalNugetServer with 
        //        | None -> failwith "local nuget server is not defined"
        //        | Some localNugetServer -> 
        //            { PreviousMsgs = [ !^ (Forker.Msg.PublishToLocalNugetServer localNugetServer) ]
        //              Action = 
        //                fun role ->
        //                    none }

    let run =
        Role.updateComplex roleAction

                                                                                   
            