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
open FSharp.Data

type EnvironmentConfig =
    { NugetApiKey: string
      GitHubToken: string
      GitHubReleaseUser: string }

with 
    static member DefaultValue =
        { NugetApiKey = "NugetApiKey"
          GitHubToken = "github_token"
          GitHubReleaseUser = "github_release_user" }

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
        member x.Permissions = x.Forker.Repository.Permissions

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
 

    type Config =
        { NugetPacker: NugetPacker
          EnvironmentConfig: EnvironmentConfig
          WorkingDir: string
          LoggerLevel: Logger.Level
          LocalNugetServer: LocalNugetServer option }

    with 
        member x.Workspace = Workspace x.WorkingDir
        static member DefaultValue =
            { NugetPacker = NugetPacker.DefaultValue
              EnvironmentConfig = EnvironmentConfig.DefaultValue
              WorkingDir = Directory.GetCurrentDirectory()
              LoggerLevel = Logger.Level.Minimal
              LocalNugetServer = None }

    [<RequireQualifiedAccess>]
    module Config =
        let tweak (config: Config) =
            { config with 
                LocalNugetServer = config.LocalNugetServer |> Option.map (fun localNugetServer ->
                    try 
                        Http.Request(localNugetServer.Serviceable,silentHttpErrors = true) |> ignore
                        Some localNugetServer
                    with _ ->
                        None
                ) |> Option.flatten }

        let fetchVersionController (forkerVersionController: Lazy<Forker.VersionController>) solution (config: Config) = 
            lazy 
                let task = 
                    task {
                        let workspace = Workspace config.WorkingDir 
                        let forkerVersionController = forkerVersionController.Value

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
                task.Result     


    [<RequireQualifiedAccess>]
    type Msg =
        | Forker of Forker.Msg
        | EnsureGitChangesAllPushedAndInDefaultBranch
        | NextRelease

    type TargetState =
        { ForkerTargetState: Forker.TargetState
          EnsureGitChangesAllPushedAndInDefaultBranch: BoxedState
          NextRelease: BoxedState }

    [<RequireQualifiedAccess>]
    module TargetState =
        let init = 
            { ForkerTargetState = Forker.TargetState.init
              EnsureGitChangesAllPushedAndInDefaultBranch = State.Init
              NextRelease = State.Init }


    type Role =
        { Forker: Forker.Role
          OfficalNugetServer: OfficalNugetServer
          TargetState: TargetState
          LocalNugetServer: LocalNugetServer option
          VersionController: Lazy<VersionController> }
    with 

        member x.NugetPacker = x.Forker.NugetPacker

        member x.Workspace = x.Forker.Workspace

        member x.Solution = x.Forker.Solution

        member x.GitHubData = x.VersionController.Value.GitHubData

        interface IRole<TargetState> 

    [<RequireQualifiedAccess>]
    module Role =
        let nextVersion (role: Role) =
            let versionController = role.VersionController.Value

            let currentVersion = VersionController.currentVersion versionController

            let tbdReleaseNotes = versionController.TbdReleaseNotes

            let versionFromTbdReleaseNotes = versionController.VersionFromTbdReleaseNotes
            
                    
            if tbdReleaseNotes.Notes.IsEmpty
            then failwith "Please write release notes of new version first"

            match currentVersion with 
            | None ->
                match versionFromTbdReleaseNotes.PreRelease with
                | Some prerelease -> 
                    SemVerInfo.normalizeAlpha versionFromTbdReleaseNotes
                    |> SemVerInfo.nextBetaVersion
                | None -> versionFromTbdReleaseNotes
                        
            | Some currentVersion ->
                if versionFromTbdReleaseNotes < currentVersion 
                then SemVerInfo.nextBetaVersion currentVersion                       
                else 
                    match versionFromTbdReleaseNotes.PreRelease with 
                    | Some _ -> 
                        SemVerInfo.normalizeAlpha versionFromTbdReleaseNotes
                        |> SemVerInfo.nextBetaVersion

                    | None -> versionFromTbdReleaseNotes  


        let nextReleaseNotes (role: Role) =
            let versionController = role.VersionController.Value
            versionController.TbdReleaseNotes
            |> ReleaseNotes.updateWithSemVerInfo (nextVersion role)
            |> ReleaseNotes.updateDateToToday   

        let internal writeReleaseNotesToNextVersionAndPushToRemoteRepository role =
            let versionController = role.VersionController.Value

            let nextVersion = nextVersion role

            let releaseNotesFile = versionController.ReleaseNotesFile

            let releaseNotesWithNextVersion = ReleaseNotes.updateWithSemVerInfo nextVersion versionController.TbdReleaseNotes

            ReleaseNotes.writeToNext versionController.ReleaseNotesFile releaseNotesWithNextVersion

            Workspace.git (sprintf "add %s" releaseNotesFile) versionController.Workspace |> ignore

            Commit.exec versionController.WorkingDir (sprintf "Bump version to %s" (SemVerInfo.normalize nextVersion))

            Branches.push versionController.WorkingDir 


    let create (config: Config) =
        let config = Config.tweak config
        let forker = Forker.create config.LoggerLevel config.Workspace config.EnvironmentConfig.GitHubToken config.NugetPacker

        let versionController = Config.fetchVersionController forker.VersionController forker.Solution config
        { OfficalNugetServer = { ApiEnvironmentName = config.EnvironmentConfig.NugetApiKey }                
          TargetState = TargetState.init
          VersionController = versionController
          Forker = forker
          LocalNugetServer = config.LocalNugetServer }

    let upcastMsg (forkerMsg: Forker.Msg) = Msg.Forker forkerMsg

    let (!^) (forkerMsg: Forker.Msg) = Msg.Forker forkerMsg
    let (!^^) (nonGitMsg: NonGit.Msg) = !^ (Forker.upcastMsg nonGitMsg)

    let private roleAction role = function 
        | Msg.Forker forkerMsg -> 
            { PreviousMsgs = []
              Action = MapChild (fun role -> Forker.run forkerMsg role.Forker)}

        | Msg.EnsureGitChangesAllPushedAndInDefaultBranch ->
            { PreviousMsgs = []
              Action = 
                MapState (fun role ->
                    let githubData = role.GitHubData
                    //match githubData.IsInDefaultBranch with 
                    //| true ->
                    match (Workspace.repoState role.Workspace) with 
                    | RepoState.Changed -> failwith "Please push all changes to git server before you draft new a release"
                    | RepoState.None -> none
                    //| false ->
                        //failwithf "Please checkout %s to default branch %s first" githubData.BranchName githubData.DefaultBranch 
                )
            }
                
        | Msg.NextRelease ->
            let nextReleaseNotes = Role.nextReleaseNotes role

            { PreviousMsgs = 
                let baseMsgs = [ !^^ NonGit.Msg.Test; Msg.EnsureGitChangesAllPushedAndInDefaultBranch ]
                match role.LocalNugetServer with 
                | Some localNugetServer -> !^(Forker.Msg.Pack nextReleaseNotes) :: baseMsgs
                | None -> baseMsgs

              Action = 
                MapState (fun role ->

                    let currentVersion = 
                        VersionController.currentVersion role.VersionController.Value
                    logger.CurrentVersion currentVersion

                    let nextVersion = Role.nextVersion role

                    logger.Important "Next version is %s" (SemVerInfo.normalize nextVersion)

                    Role.writeReleaseNotesToNextVersionAndPushToRemoteRepository role

                    [ yield GitHubData.draftAndPublishWithNewRelease nextReleaseNotes role.GitHubData
                      match role.LocalNugetServer with 
                      | Some localNugetServer -> 
                            let newPackages = role.Forker.TargetState.Pack |> State.getResult
                            yield LocalNugetServer.publish newPackages localNugetServer 
                      | None -> ()]
                    |> Async.Parallel
                    |> Async.RunSynchronously
                    |> ignore

                    none 
                )
            }

    let run =
        Role.updateComplex roleAction

                                                                                   
            