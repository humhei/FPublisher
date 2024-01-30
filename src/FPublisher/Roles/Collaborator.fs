namespace FPublisher.Roles
open FPublisher.Nuget
open Fake.Core
open FPublisher
open FPublisher.GitHub
open FSharp.Control.Tasks.V2.ContextInsensitive
open FPublisher.FakeHelper.Build
open Fake.IO.FileSystemOperators
open Primitives
open FPublisher.Git
open System.IO
open FPublisher.FakeHelper
open Fake.Tools.Git
open Fake.IO
open Fake.Api
open FPublisher.Utils
open FPublisher.Solution
open FPublisher.Nuget.NugetPacker
open System
open Fake.DotNet
open FPublisher.Nuget.Nuget

#nowarn "0064"

type EnvironmentConfig =
    { NugetApiKey: string
      GitHubToken: string
      GitHubReleaseUser: string }

with
    static member DefaultValue =
        { NugetApiKey = "nuget_api_key"
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
          EnvironmentConfig: EnvironmentConfig
          BranchName: string
          CommitHashRemote: string
          CommitHashLocal: string }
    with
        member x.Permissions = x.Forker.Repository.Permissions

        member x.Repository = x.Forker.Repository

        member x.DefaultBranch = x.Repository.DefaultBranch

        member x.IsInDefaultBranch = x.DefaultBranch = x.BranchName

        member x.CheckInDefaultBranch() =
            if not x.IsInDefaultBranch
            then failwithf "Should in default branch %s, but currrent in branch %s" x.DefaultBranch x.BranchName

        member x.Owner = x.Repository.Owner

        member x.LoginName = x.Owner.Login

        member x.RepoName = x.Repository.Name



    [<RequireQualifiedAccess>]
    module GitHubData =
        let githubToken githubData =
            Environment.environVarOrFail githubData.EnvironmentConfig.GitHubToken

        let releaseUserName githubData =
            Environment.environVarOrFail githubData.EnvironmentConfig.GitHubReleaseUser

        let draftAndPublishWithNewRelease files (releaseNotes: ReleaseNotes.ReleaseNotes) (githubData: GitHubData) =
            GitHub.createClientWithToken (githubToken githubData)
            |> GitHubClient.draftAndPublishWithNewRelease files (releaseUserName githubData) githubData.RepoName releaseNotes

        let fetch forkerGitHubData workspace (environmentConfig: EnvironmentConfig) = task {
            let branchName = Workspace.branchName workspace

            return
                { Forker = forkerGitHubData
                  EnvironmentConfig =  environmentConfig
                  BranchName = branchName
                  CommitHashLocal = Workspace.commitHashLocal branchName workspace
                  CommitHashRemote = Workspace.commitHashRemote branchName workspace }
        }

        let isInDefaultRepo workspace (githubData: GitHubData) =
            let gitUrl = Workspace.gitUrl workspace
            githubData.Repository.CloneUrl = gitUrl
            || githubData.Repository.GitUrl = gitUrl

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
          LocalNugetServer: NugetServer option
          LocalNugetPackagesFolder: string option }

    with
        member x.Workspace = Workspace x.WorkingDir

        static member DefaultValue =
            { NugetPacker = NugetPacker.DefaultValue
              EnvironmentConfig = EnvironmentConfig.DefaultValue
              WorkingDir = Directory.GetCurrentDirectory()
              LoggerLevel = Logger.Level.Minimal
              LocalNugetServer = None
              LocalNugetPackagesFolder = None }

    [<RequireQualifiedAccess>]
    module Config =
        let tweak (config: Config) =
            { config with
                LocalNugetServer = config.LocalNugetServer |> Option.map NugetServer.ping
                |> Option.flatten }

        let fetchVersionController (forkerVersionController: Lazy<Forker.VersionController>) solution (config: Config) =
            lazy
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
                |> Task.getResult


    [<RequireQualifiedAccess>]
    type Target =
        | Forker of Forker.Target
        | EnsureGitChangesAllPushedAndInDefaultBranch
        | NextRelease

    type TargetStates =
        { Forker: Forker.TargetStates
          EnsureGitChangesAllPushedAndInDefaultBranch: BoxedTargetState
          NextRelease: BoxedTargetState }

    [<RequireQualifiedAccess>]
    module TargetStates =
        let init =
            { Forker = Forker.TargetStates.init
              EnsureGitChangesAllPushedAndInDefaultBranch = TargetState.Init
              NextRelease = TargetState.Init }


    type Role =
        { Forker: Forker.Role
          OfficalNugetServer: OfficalNugetServer
          TargetStates: TargetStates
          LocalNugetServer: NugetServer option
          VersionController: Lazy<VersionController> }
    with

        member x.NugetPacker = x.Forker.NugetPacker

        member x.Workspace = x.Forker.Workspace

        member x.ReleaseNotesFile = x.Workspace.ReleaseNotesFile

        member x.Solution = x.Forker.Solution

        member x.GitHubData = x.VersionController.Value.GitHubData

        member x.NonGit = x.Forker.NonGit

        interface IRole<TargetStates>

    [<RequireQualifiedAccess>]
    module Role =
        let nextVersion (role: Role) =
            let versionController = role.VersionController.Value

            let currentVersion = VersionController.currentVersion versionController


            let versionFromTbdReleaseNotes = versionController.VersionFromTbdReleaseNotes


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

            let tbdReleaseNotes = versionController.TbdReleaseNotes

            if tbdReleaseNotes.Notes.IsEmpty
            then failwith "Please write release notes of new version first"

            tbdReleaseNotes
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
        let forker = Forker.create config.LoggerLevel config.LocalNugetServer config.LocalNugetPackagesFolder config.Workspace config.NugetPacker

        let versionController = Config.fetchVersionController forker.VersionController forker.Solution config
        { OfficalNugetServer = { ApiEnvironmentName = config.EnvironmentConfig.NugetApiKey }
          TargetStates = TargetStates.init
          VersionController = versionController
          Forker = forker
          LocalNugetServer = config.LocalNugetServer }

    [<AutoOpen>]
    module SRTPMsgs =

        type Ext = Ext
            with
                static member Bar (ext : Ext, nonGit : NonGit.Target) =
                    Forker.upcastMsg nonGit
                    |> Target.Forker

                static member Bar (ext : Ext, forker : Forker.Target) =
                    Target.Forker forker

    let inline upcastMsg msg =
        ((^b or ^a) : (static member Bar : ^b * ^a -> Target) (Ext, msg))

    let inline private (!^) msg =
        ((^b or ^a) : (static member Bar : ^b * ^a -> Target) (Ext, msg))



    let private roleAction (role: Role) = function
        | Target.Forker forkerMsg ->
            { DependsOn = []
              Action = MapChild (fun role -> 
                let newChildRole = Forker.run forkerMsg role.Forker
                { role with 
                    TargetStates = 
                        { role.TargetStates with Forker = newChildRole.TargetStates }
                    Forker = newChildRole
                }
            )}

        | Target.EnsureGitChangesAllPushedAndInDefaultBranch ->
            { DependsOn = []
              Action =
                MapState (fun role ->
                    if role.GitHubData.IsInDefaultBranch
                    then
                        let diffFilter diffs =
                            let execludes =
                                [ "RELEASE_NOTES.md"
                                  "Paket.Restore.targets" ]
                            diffs |> List.filter (fun diff ->
                                let name = Path.GetFileName diff
                                execludes |> List.exists (fun execlude ->
                                    String.equalIgnoreCaseAndEdgeSpace execlude name
                                ) |> not
                            )

                        match (Workspace.repoStateWith diffFilter role.Workspace) with
                        | RepoState.Changed -> failwith "Please push all changes to git server before you draft new a release"
                        | RepoState.None -> none
                    else
                        let githubData = role.GitHubData
                        failwithf "Please checkout %s to default branch %s first" githubData.BranchName githubData.DefaultBranch
                )
            }

        | Target.NextRelease ->

            /// check environments first
            let _ = GitHubData.githubToken role.GitHubData
            let _ = GitHubData.releaseUserName role.GitHubData

            let nextReleaseNotes = Role.nextReleaseNotes role

            { DependsOn =
                [ !^ (NonGit.Target.Build_Release (DotNet.BuildOptions.setVersion nextReleaseNotes.SemVer))
                  Target.EnsureGitChangesAllPushedAndInDefaultBranch
                  !^ (NonGit.Target.Zip (List.filter Project.existFullFramework role.Solution.CliProjects )) ]

              Action =
                MapState (fun role ->

                    let currentVersion =
                        VersionController.currentVersion role.VersionController.Value

                    let zipOutputs = TargetState.getResult role.NonGit.TargetStates.Zip 

                    logger.CurrentVersion currentVersion

                    let nextVersion = Role.nextVersion role

                    logger.Important "Next version is %s" (SemVerInfo.normalize nextVersion)

                    Role.writeReleaseNotesToNextVersionAndPushToRemoteRepository role

                    [ GitHubData.draftAndPublishWithNewRelease zipOutputs nextReleaseNotes role.GitHubData ]
                    |> Async.Parallel
                    |> Async.RunSynchronously
                    |> ignore

                    none
                )
            }

    let run =
        Role.Run roleAction