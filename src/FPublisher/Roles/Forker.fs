namespace FPublisher.Roles
open FPublisher
open FPublisher.Solution
open FPublisher.Nuget
open FPublisher.Nuget.NugetPacker
open Fake.IO.FileSystemOperators
open Primitives
open Fake.Core
open Fake.IO
open FPublisher.GitHub
open FPublisher.FakeHelper.Build
open Octokit
open FSharp.Control.Tasks.V2.ContextInsensitive
open FPublisher.Git
open System
open FPublisher.Utils
open Fake.DotNet
open FPublisher.Nuget.Nuget
open System.IO



[<RequireQualifiedAccess>]
module Forker =

    type GitHubData = CommonGitHubData

    type VersionController =
        { Workspace: Workspace
          VersionFromOfficalNugetServer: SemVerInfo option
          LastReleaseNotes: ReleaseNotes.ReleaseNotes option
          TbdReleaseNotes: ReleaseNotes.ReleaseNotes
          GitHubData: GitHubData }

    with

        member x.VersionFromLastReleaseNotes = x.LastReleaseNotes |> Option.map (fun releaseNotes -> releaseNotes.SemVer)

        member x.VersionFromTbdReleaseNotes = 
            x.TbdReleaseNotes.SemVer

        member x.ReleaseNotesFile = x.Workspace.WorkingDir </> "RELEASE_NOTES.md"

        member x.WorkingDir = x.Workspace.WorkingDir


    [<RequireQualifiedAccess>]
    module VersionController =

        let versionFromLocalNugetServer solution (localNugetServer: NugetServer) (versionController: VersionController) = async {
            logger.Infots "Begin fetch version from local nuget server"
            let! versionFromLocalNugetServer =
                Solution.getLastVersion solution localNugetServer
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
                    let! githubData = CommonGitHubData.fetch workspace
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
    type Target =
        | NonGit of NonGit.Target
        | Pack of (FPublisher.DotNet.PackOptions -> FPublisher.DotNet.PackOptions) * ReleaseNotes.ReleaseNotes 
        | PublishToLocalNugetServer

    let (!^) (nonGitMsg: NonGit.Target) = Target.NonGit nonGitMsg

    let upcastMsg (nonGitMsg: NonGit.Target) = Target.NonGit nonGitMsg

    type TargetStates =
        { NonGit: NonGit.TargetStates
          Pack: BoxedTargetState
          PublishToLocalNugetServer: BoxedTargetState }

    [<RequireQualifiedAccess>]
    module TargetStates =
        let init =
            { NonGit = NonGit.TargetStates.init
              Pack = TargetState.Init
              PublishToLocalNugetServer = TargetState.Init }

    type Role =
        { NonGit: NonGit.Role
          TargetStates: TargetStates
          NugetPacker: NugetPacker
          LocalNugetServer: NugetServer option
          VersionController: Lazy<VersionController> }
    with
        member x.Solution = x.NonGit.Solution

        member x.Workspace = x.NonGit.Workspace

        member x.GitHubData = x.VersionController.Value.GitHubData

        interface IRole<TargetStates>

    [<RequireQualifiedAccess>]
    module Role =
        let currentVersion versionFromLocalNugetServer (role: Role) =
            VersionController.currentVersion versionFromLocalNugetServer role.VersionController.Value

        let nextLocalNugetVersion versionFromLocalNugetServer (role: Role) =
            let versionController = role.VersionController.Value
            let currentVersion = currentVersion versionFromLocalNugetServer role
            let tbdVersion =  versionController.VersionFromTbdReleaseNotes

            match currentVersion with
            | Some version -> SemVerInfo.nextBuildVersion (max version tbdVersion)
            | None -> SemVerInfo.nextBuildVersion (tbdVersion)


        let nextLocalNugetReleaseNotes versionFromLocalNugetServer role =
            let nextVersion = nextLocalNugetVersion versionFromLocalNugetServer role
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

    let create loggerLevel localNugetServerOp localPackagesFolder workspace nugetPacker =
        let nonGit = NonGit.create loggerLevel localNugetServerOp localPackagesFolder  workspace

        { NonGit = nonGit
          NugetPacker = nugetPacker
          TargetStates = TargetStates.init
          VersionController = VersionController.fetch nonGit.Solution nonGit.Workspace
          LocalNugetServer = localNugetServerOp }


    let private roleAction (role: Role) = function
        | Target.NonGit nonGitMsg ->
            { DependsOn = []
              Action = MapChild (fun (role: Role) -> 
                let newChildRole = NonGit.run nonGitMsg role.NonGit 
                { role with 
                    TargetStates = 
                        { role.TargetStates with NonGit = newChildRole.TargetStates }
                    NonGit = newChildRole
                }
            )}

        | Target.Pack (setParams, releaseNotes) ->
            let buildOps =
                let githubData = role.GitHubData
                let packOps = setParams FPublisher.DotNet.PackOptions.DefaultValue
                NugetPacker.updatePackOptions 
                    githubData
                    releaseNotes
                    packOps
                    role.NugetPacker
                |> DotNet.PackOptions.asFakeBuildOptions

            { DependsOn = [!^ (NonGit.Target.Build_Release (fun _ -> buildOps)); !^ NonGit.Target.Test]
              Action = MapState (fun role ->
                let githubData = role.GitHubData
                let outputDirectory = Path.GetTempPath() </> Path.GetRandomFileName()

                Directory.ensure outputDirectory

                let setParams (ops: FPublisher.DotNet.PackOptions) =
                    let ops = setParams ops
                    { ops with 
                        Configuration = DotNet.BuildConfiguration.Release
                        NoBuild = true 
                        Version = Some releaseNotes.SemVer.AsString
                        OutputPath = Some outputDirectory }

                let newPackages =
                    NugetPacker.pack
                        role.Solution
                        githubData
                        releaseNotes
                        setParams
                        role.NugetPacker
                newPackages
                |> box
                )
            }


        | Target.PublishToLocalNugetServer ->

            match role.LocalNugetServer with
            | Some localNugetServer ->
                let versionFromLocalNugetServer = Role.versionFromLocalNugetServer role |> Async.RunSynchronously
                let nextReleaseNotes = Role.nextLocalNugetReleaseNotes versionFromLocalNugetServer role

                { DependsOn = [ Target.Pack (id, nextReleaseNotes) ]
                  Action = MapState (fun role ->
                    let packResult: PackResult = TargetState.getResult role.TargetStates.Pack

                    let currentVersion = VersionController.currentVersion versionFromLocalNugetServer role.VersionController.Value

                    logger.CurrentVersion currentVersion

                    let nextVersion = Role.nextLocalNugetVersion versionFromLocalNugetServer role

                    logger.ImportantGreen "Next version is %s" (SemVerInfo.normalize nextVersion)

                    NugetServer.publish (packResult.LibraryPackagePaths @ packResult.CliPackagePaths) localNugetServer |> Async.RunSynchronously
                    none
                    )
                }
            | None -> failwithf "local nuget server is not defined"

    let run =
        IRole.Run roleAction
