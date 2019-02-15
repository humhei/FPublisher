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
                let repoName = Workspace.repoName workspace
                GitHubClient.repository repoName client

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

        let versionFromLocalNugetServer solution (localNugetServer: LocalNugetServer) (versionController: VersionController) = async {
            logger.Infots "Begin fetch version from local nuget server"
            let! versionFromLocalNugetServer =
                NugetServer.getLastVersion solution localNugetServer.AsNugetServer
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
        | Pack of nextReleaseNotes: ReleaseNotes.ReleaseNotes * packageIdSuffix: string
        | PublishToLocalNugetServer of LocalNugetServer

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
          VersionController: Lazy<VersionController> }
    with
        member x.Solution = x.NonGit.Solution

        member x.Workspace = x.NonGit.Workspace

        member x.GitHubData = x.VersionController.Value.GitHubData

        interface IRole<TargetState>

    [<RequireQualifiedAccess>]
    module Role =
        let nextVersion versionFromLocalNugetServer (role: Role) =
            let versionController = role.VersionController.Value
            let currentVersion = VersionController.currentVersion versionFromLocalNugetServer versionController

            [ currentVersion
              Some (versionController.VersionFromTbdReleaseNotes) ]
            |> List.choose id
            |> List.max
            |> SemVerInfo.nextBuildVersion


        let nextVersionWithFetch localNugetServer (role: Role) = async {
            let! versionFromLocalNugetServer = VersionController.versionFromLocalNugetServer role.Solution localNugetServer role.VersionController.Value
            return nextVersion versionFromLocalNugetServer role
        }

        let nextReleaseNotes versionFromLocalNugetServer role =
            let nextVersion = nextVersion versionFromLocalNugetServer role
            VersionController.nextReleaseNotes nextVersion role.VersionController.Value

        let nextReleaseNotesWithFetch localNugetServer (role: Role) = async {
            let! versionFromLocalNugetServer = VersionController.versionFromLocalNugetServer role.Solution localNugetServer role.VersionController.Value
            return nextReleaseNotes versionFromLocalNugetServer role
        }



    let create loggerLevel workspace nugetPacker =
        let nonGit = NonGit.create loggerLevel workspace

        { NonGit = nonGit
          NugetPacker = nugetPacker
          TargetState = TargetState.init
          VersionController = VersionController.fetch nonGit.Solution nonGit.Workspace }


    let private roleAction (role: Role) = function
        | Msg.NonGit nonGitMsg ->
            { PreviousMsgs = []
              Action = MapChild (fun (role: Role) -> NonGit.run nonGitMsg role.NonGit )}

        | Msg.Pack (nextReleaseNotes,packageIDSuffix) ->
            { PreviousMsgs = [!^ NonGit.Msg.Build; !^ NonGit.Msg.Test]
              Action = MapState (fun role ->
                let githubData = role.GitHubData

                NugetPacker.pack
                    role.Solution
                    packageIDSuffix
                    true
                    true
                    githubData.Topics
                    githubData.License
                    githubData.Repository
                    nextReleaseNotes
                    role.NugetPacker
                |> box
                )
            }

        | Msg.PublishToLocalNugetServer localNugetServer ->

            match LocalNugetServer.ping localNugetServer with
            | Some _ -> ()
            | None -> failwithf "Can not access local nuget server %s" localNugetServer.Serviceable

            let versionFromLocalNugetServer =
                VersionController.versionFromLocalNugetServer role.Solution localNugetServer role.VersionController.Value
                |> Async.RunSynchronously

            let currentVersion = VersionController.currentVersion versionFromLocalNugetServer role.VersionController.Value
            logger.CurrentVersion currentVersion

            let nextReleaseNotes = Role.nextReleaseNotes versionFromLocalNugetServer role

            logger.Important "next build version %s" (SemVerInfo.normalize nextReleaseNotes.SemVer)

            { PreviousMsgs = [ Msg.Pack (nextReleaseNotes, "")]
              Action = MapState (fun role ->
                let newPackages = State.getResult role.TargetState.Pack
                NugetServer.publish newPackages localNugetServer.AsNugetServer |> Async.RunSynchronously
                none
                )
            }

    let run = Role.updateComplex roleAction