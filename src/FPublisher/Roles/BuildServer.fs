namespace FPublisher.Roles
open Primitives
open Microsoft.Build.Logging.StructuredLogger
open Fake.Core
open Fake.BuildServer
open FPublisher.FakeHelper.Build
open FPublisher.Nuget
open Fake.IO
open Fake.IO.FileSystemOperators
open System.IO
open FPublisher.FakeHelper
open FPublisher
open FPublisher.FakeHelper.CommandHelper

#nowarn "0064"

[<RequireQualifiedAccess>]
module BuildServer =
    type Config =
        { ArtifactsName: string
          NugetPacker: NugetPacker
          EnvironmentConfig: EnvironmentConfig
          WorkingDir: string
          LoggerLevel: Logger.Level
          LocalNugetServer: LocalNugetServer option }
    with
        static member DefaultValue =
            let collaborator = Collaborator.Config.DefaultValue
            { ArtifactsName = "build_output"
              NugetPacker = collaborator.NugetPacker
              EnvironmentConfig = collaborator.EnvironmentConfig
              WorkingDir = collaborator.WorkingDir
              LoggerLevel = collaborator.LoggerLevel
              LocalNugetServer = collaborator.LocalNugetServer }

        member internal x.AsCollaborator: Collaborator.Config =
            { NugetPacker = x.NugetPacker
              EnvironmentConfig = x.EnvironmentConfig
              WorkingDir = x.WorkingDir
              LoggerLevel = x.LoggerLevel
              LocalNugetServer = x.LocalNugetServer }

    [<RequireQualifiedAccess>]
    type Msg =
        | Collaborator of Collaborator.Msg
        | RunCI

    type TargetState =
        { Collaborator: Collaborator.TargetState
          RunCI: BoxedState }

    type Role =
        { Collaborator: Collaborator.Role
          TargetState: TargetState
          ArtifactsName: string
          MajorCI: BuildServer }
    with
        member x.Workspace = x.Collaborator.Workspace

        member x.VersionController = x.Collaborator.VersionController

        member x.ReleaseNotesFile = x.Collaborator.ReleaseNotesFile

        member x.ArtifactsDirPath =
            x.Workspace.WorkingDir </> "build_output"
            |> Directory.ensureReturn

        interface IRole<TargetState>

    [<RequireQualifiedAccess>]
    module Role =
        let afterDraftedNewRelease role =
            match role.MajorCI with
            | BuildServer.AppVeyor ->
                let prHeadRepoName = AppVeyor.Environment.PullRequestRepoName
                role.Collaborator.GitHubData.IsInDefaultBranch && String.isNullOrEmpty prHeadRepoName
            | _ ->
                failwith "not implemted"

        let nextReleaseNotes role =
            if afterDraftedNewRelease role then ReleaseNotes.loadLast role.ReleaseNotesFile
            else
                let nextVersion =
                    let forkerNextVersionIgnoreLocalNugetServer = (Forker.Role.nextVersion None role.Collaborator.Forker)
                    let mainVersionText = SemVerInfo.mainVersionText forkerNextVersionIgnoreLocalNugetServer
                    (mainVersionText + "-build." + AppVeyor.Environment.BuildNumber)
                    |> SemVerInfo.parse

                ReleaseNotes.loadTbd role.ReleaseNotesFile
                |> ReleaseNotes.updateWithSemVerInfo nextVersion
                |> ReleaseNotes.updateDateToToday
                |> Some

    let create (config: Config) =
        BuildServer.install [
            AppVeyor.Installer
            Travis.Installer
        ]


        { Collaborator = Collaborator.create config.AsCollaborator
          TargetState =
            { Collaborator = Collaborator.TargetState.init
              RunCI = State.Init }
          MajorCI = BuildServer.AppVeyor
          ArtifactsName = config.ArtifactsName
        }

    [<AutoOpen>]
    module SRTPMsgs =

        type Ext = Ext
            with
                static member Bar (ext : Ext, nonGit : NonGit.Msg) =
                    Collaborator.upcastMsg nonGit
                    |> Msg.Collaborator

                static member Bar (ext : Ext, forker : Forker.Msg) =
                    Collaborator.upcastMsg forker
                    |> Msg.Collaborator

                static member Bar (ext : Ext, collaborator : Collaborator.Msg) =
                    collaborator
                    |> Msg.Collaborator

    let inline upcastMsg msg =
        ((^b or ^a) : (static member Bar : ^b * ^a -> Msg) (Ext, msg))

    let inline private (!^) msg =
        ((^b or ^a) : (static member Bar : ^b * ^a -> Msg) (Ext, msg))


    let private circleCIBuildNumber = Environment.environVar "CIRCLE_BUILD_NUM"

    let private roleAction role = function
        | Msg.Collaborator collaboratorMsg ->
            { PreviousMsgs = []
              Action = MapChild (fun role ->
                    Collaborator.run collaboratorMsg role.Collaborator
                )
            }
        | Msg.RunCI ->

            match BuildServer.buildServer with
            | BuildServer.LocalBuild when String.isNullOrEmpty circleCIBuildNumber -> failwith "Expect buildServer context, but currently run in local context"
            | buildServer when buildServer = role.MajorCI  ->


                match Role.nextReleaseNotes role with
                | Some nextReleaseNotes ->
                    let appveyor = platformTool "appveyor"
                    exec appveyor "./" ["-Version"; nextReleaseNotes.AssemblyVersion + ".{Build}"]

                    let isAfterDraftedNewRelease = Role.afterDraftedNewRelease role

                    { PreviousMsgs = [!^ NonGit.Msg.Test; !^ (Forker.Msg.Pack (nextReleaseNotes, "")); ]
                      Action = MapState (fun role ->
                        let newPackages = role.Collaborator.Forker.TargetState.Pack |> State.getResult
                        newPackages |> Shell.copyFiles role.ArtifactsDirPath
                        if isAfterDraftedNewRelease then
                            OfficalNugetServer.publish newPackages role.Collaborator.OfficalNugetServer
                            |> Async.RunSynchronously
                    )}
                | None -> failwith "Pack nuget packages need last releaseNotes info. But we can't load it"

            | _ ->
                /// run tests only
                { PreviousMsgs = [ !^ NonGit.Msg.Test ]
                  Action = MapState ignore }

    let run =
        Role.updateComplex roleAction
