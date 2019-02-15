namespace FPublisher.Roles
open Primitives
open Fake.Core
open Fake.BuildServer
open FPublisher.FakeHelper.Build
open FPublisher.Nuget
open Fake.IO
open Fake.IO.FileSystemOperators
open FPublisher.FakeHelper
open FPublisher
open FPublisher.FakeHelper.CommandHelper
open Fake.SystemHelper
open FPublisher.GitHub

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

        member x.GitHubData = x.Collaborator.GitHubData

        member x.ArtifactsDirPath =
            x.Workspace.WorkingDir </> "build_output"
            |> Directory.ensureReturn

        interface IRole<TargetState>


    let create (config: Config) =
        BuildServer.install [
            AppVeyor.Installer
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

    let private roleAction (role: Role) = function
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

                let isJustAfterDraftedNewRelease (role: Role) =
                    let repoTagName = AppVeyor.Environment.RepoTagName
                    if String.isNullOrEmpty repoTagName
                    then false
                    else
                        let lastReleaseNotes = ReleaseNotes.loadLast role.ReleaseNotesFile

                        match lastReleaseNotes with
                        | Some lastReleaseNotes ->
                            lastReleaseNotes.NugetVersion = repoTagName
                        | None -> false



                let nextReleaseNotes role =
                    if isJustAfterDraftedNewRelease role then ReleaseNotes.loadLast role.ReleaseNotesFile
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


                match nextReleaseNotes role with
                | Some nextReleaseNotes ->
                    let appveyor = platformTool "appveyor"
                    exec appveyor "./" ["UpdateBuild"; "-Version"; SemVerInfo.normalize nextReleaseNotes.SemVer ]

                    let isJustAfterDraftedNewRelease = isJustAfterDraftedNewRelease role

                    { PreviousMsgs = [!^ NonGit.Msg.Test; !^ (Forker.Msg.Pack (nextReleaseNotes, "")); ]
                      Action = MapState (fun role ->
                        let newPackages = role.Collaborator.Forker.TargetState.Pack |> State.getResult
                        newPackages |> Shell.copyFiles role.ArtifactsDirPath
                        if isJustAfterDraftedNewRelease then
                            let githubData = role.GitHubData
                            if githubData.BranchName = "HEAD" then

                                if Collaborator.GitHubData.isInDefaultRepo role.Workspace  githubData
                                then
                                    logger.Importantts "Begin publish nuget packages to offical nuget server"
                                    OfficalNugetServer.publish newPackages role.Collaborator.OfficalNugetServer
                                    |> Async.RunSynchronously
                                    logger.Importantts "End publish nuget packages to offical nuget server"
                            else failwith "Branch name should be HEAD when trigger from tag"

                    )}
                | None -> failwith "Pack nuget packages need last releaseNotes info. But we can't load it"

            | _ ->

                /// run tests only
                { PreviousMsgs = [ !^ NonGit.Msg.Test ]
                  Action = MapState ignore }

    let run =
        Role.updateComplex roleAction
