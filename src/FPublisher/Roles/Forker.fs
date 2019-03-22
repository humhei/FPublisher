namespace FPublisher.Roles
open FPublisher
open FPublisher.Nuget
open Fake.IO.FileSystemOperators
open Primitives
open Fake.Core
open FPublisher.GitHub
open FPublisher.FakeHelper.Build
open Octokit
open FSharp.Control.Tasks.V2.ContextInsensitive
open FPublisher.Git
open System
open FPublisher.Utils
open Fake.DotNet



[<RequireQualifiedAccess>]
module Forker =

    type GitHubData =
        { Topics: Topics
          Repository: Repository
          License: RepositoryContentLicense }


    [<RequireQualifiedAccess>]
    module GitHubData =
        let fetch workspace = task {

            let! client = GitHubClient.createWithoutToken()

            let! repository =
                let repoFullName = Workspace.repoFullName workspace
                GitHubClient.repository repoFullName client

            let! license =
                let logger = repository.Owner.Login
                client.Repository.GetLicenseContents(logger, repository.Name)

            let! topics = Repository.topicsAsync repository

            return
                { Topics = topics
                  Repository = repository
                  License = license }
        }

    type VersionController =
        { Workspace: Workspace
          VersionFromOfficalNugetServer: SemVerInfo option
          LastReleaseNotes: ReleaseNotes.ReleaseNotes option
          TbdReleaseNotes: ReleaseNotes.ReleaseNotes
          GitHubData: GitHubData }

    with

        member x.VersionFromLastReleaseNotes = x.LastReleaseNotes |> Option.map (fun releaseNotes -> releaseNotes.SemVer)

        member x.VersionFromTbdReleaseNotes = x.TbdReleaseNotes.SemVer

        member x.ReleaseNotesFile = x.Workspace.WorkingDir </> "RELEASE_NOTES.md"

        member x.WorkingDir = x.Workspace.WorkingDir


    [<RequireQualifiedAccess>]
    module VersionController =

        let versionFromLocalNugetServer solution (localNugetServer: NugetServer) (versionController: VersionController) = async {
            logger.Infots "Begin fetch version from local nuget server"
            let! versionFromLocalNugetServer =
                NugetServer.getLastVersion solution localNugetServer
            logger.Infots "End fetch version from local nuget server"
            return versionFromLocalNugetServer
        }

        let currentVersion (versionFromLocalNugetServer: SemVerInfo option) versionController =
            let versions =
                [ versionController.VersionFromOfficalNugetServer
                  versionFromLocalNugetServer
                  versionController.VersionFromLastReleaseNotes ]
                |> List.choose id

            if versions.Length = 0 then None
            else Some (List.max versions)

        let nextReleaseNotes nextVersion versionController =
            versionController.TbdReleaseNotes
            |> ReleaseNotes.updateWithSemVerInfo (nextVersion)
            |> ReleaseNotes.updateDateToToday

        let fetch solution workspace =
            lazy
                task {
                    logger.Infots "Begin fetch forker versionController"
                    logger.Infots "Begin fetch github data"
                    let! githubData = GitHubData.fetch workspace
                    logger.Infots "End fetch github data"

                    let tbdReleaseNotes,lastReleaseNotes =
                        let releaseNotesFile = workspace.WorkingDir </> "RELEASE_NOTES.md"
                        ReleaseNotes.loadTbd releaseNotesFile, ReleaseNotes.loadLast releaseNotesFile

                    logger.Infots "Begin fetch version from nuget server"
                    let! versionFromOfficalNugetServer = Solution.lastVersionFromOfficalNugetServer solution
                    logger.Infots "End fetch version from nuget server"
                    logger.Infots "End fetch forker versionController"


                    let result =
                        { GitHubData = githubData
                          Workspace = workspace
                          TbdReleaseNotes = tbdReleaseNotes
                          LastReleaseNotes = lastReleaseNotes
                          VersionFromOfficalNugetServer = versionFromOfficalNugetServer }

                    logger.Info "%A" result

                    return result
                } |> Task.getResult

    [<RequireQualifiedAccess>]
    type Msg =
        | NonGit of NonGit.Msg
        | Pack of ReleaseNotes.ReleaseNotes
        | PublishToLocalNugetServer

    let (!^) (nonGitMsg: NonGit.Msg) = Msg.NonGit nonGitMsg

    let upcastMsg (nonGitMsg: NonGit.Msg) = Msg.NonGit nonGitMsg

    type TargetState =
        { NonGit: NonGit.TargetState
          Pack: BoxedState
          PublishToLocalNugetServer: BoxedState }

    [<RequireQualifiedAccess>]
    module TargetState =
        let init =
            { NonGit = NonGit.TargetState.init
              Pack = State.Init
              PublishToLocalNugetServer = State.Init }

    type Role =
        { NonGit: NonGit.Role
          TargetState: TargetState
          NugetPacker: NugetPacker
          LocalNugetServer: NugetServer option
          VersionController: Lazy<VersionController> }
    with
        member x.Solution = x.NonGit.Solution

        member x.Workspace = x.NonGit.Workspace

        member x.GitHubData = x.VersionController.Value.GitHubData

        interface IRole<TargetState>

    [<RequireQualifiedAccess>]
    module Role =
        let currentVersion versionFromLocalNugetServer (role: Role) =
            VersionController.currentVersion versionFromLocalNugetServer role.VersionController.Value

        let nextVersion versionFromLocalNugetServer (role: Role) =
            let versionController = role.VersionController.Value
            let currentVersion = currentVersion versionFromLocalNugetServer role

            match currentVersion with
            | Some version -> SemVerInfo.nextBuildVersion version
            | None -> SemVerInfo.nextBuildVersion (versionController.VersionFromTbdReleaseNotes)


        let nextReleaseNotes versionFromLocalNugetServer role =
            let nextVersion = nextVersion versionFromLocalNugetServer role
            VersionController.nextReleaseNotes nextVersion role.VersionController.Value

        let versionFromLocalNugetServer role = async {
            let version =
                role.LocalNugetServer
                |> Option.map (fun localNugetServer ->
                    VersionController.versionFromLocalNugetServer role.Solution localNugetServer role.VersionController.Value
                    |> Async.RunSynchronously
                )
                |> Option.flatten

            return version
        }

    let create loggerLevel localNugetServerOp workspace nugetPacker =
        let nonGit = NonGit.create loggerLevel workspace

        { NonGit = nonGit
          NugetPacker = nugetPacker
          TargetState = TargetState.init
          VersionController = VersionController.fetch nonGit.Solution nonGit.Workspace
          LocalNugetServer = localNugetServerOp }


    let private roleAction (role: Role) = function
        | Msg.NonGit nonGitMsg ->
            { PreviousMsgs = []
              Action = MapChild (fun (role: Role) -> NonGit.run nonGitMsg role.NonGit )}

        | Msg.Pack releaseNotes ->
            { PreviousMsgs = [!^ (NonGit.Msg.Build (Some releaseNotes.SemVer)); !^ NonGit.Msg.Test]
              Action = MapState (fun role ->
                let githubData = role.GitHubData

                let newPackages =
                    NugetPacker.pack
                        role.Solution
                        true
                        true
                        githubData.Topics
                        githubData.License
                        githubData.Repository
                        releaseNotes
                        role.NugetPacker
                newPackages
                |> box
                )
            }


        | Msg.PublishToLocalNugetServer ->

            match role.LocalNugetServer with
            | Some localNugetServer ->
                let versionFromLocalNugetServer = Role.versionFromLocalNugetServer role |> Async.RunSynchronously
                let nextReleaseNotes = Role.nextReleaseNotes versionFromLocalNugetServer role

                { PreviousMsgs = [ Msg.Pack nextReleaseNotes ]
                  Action = MapState (fun role ->
                    let packResult = State.getResult role.TargetState.Pack

                    let currentVersion = VersionController.currentVersion versionFromLocalNugetServer role.VersionController.Value

                    logger.CurrentVersion currentVersion

                    let nextVersion = Role.nextVersion versionFromLocalNugetServer role

                    logger.ImportantGreen "Next version is %s" (SemVerInfo.normalize nextVersion)

                    NugetServer.publish (packResult.LibraryPackages @ packResult.CliPackages) localNugetServer |> Async.RunSynchronously
                    none
                    )
                }
            | None -> failwithf "local nuget server is not defined"

    let run = Role.updateComplex roleAction