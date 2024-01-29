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
open FPublisher.Git
open FPublisher.Utils
open FPublisher.Solution
open FPublisher.Nuget.NugetPacker
open Fake.DotNet
open FPublisher.Nuget.Nuget


#nowarn "0064"

[<RequireQualifiedAccess>]
module BuildServer =
    type Config =
        { ArtifactsName: string
          NugetPacker: NugetPacker
          EnvironmentConfig: EnvironmentConfig
          WorkingDir: string
          LoggerLevel: Logger.Level
          LocalNugetServer: NugetServer option
          LocalNugetPackagesFolder: string option }
    with
        static member DefaultValue =
            let collaborator = Collaborator.Config.DefaultValue
            { ArtifactsName = "build_output"
              NugetPacker = collaborator.NugetPacker
              EnvironmentConfig = collaborator.EnvironmentConfig
              WorkingDir = collaborator.WorkingDir
              LoggerLevel = collaborator.LoggerLevel
              LocalNugetServer = collaborator.LocalNugetServer
              LocalNugetPackagesFolder = None }

        member internal x.AsCollaborator: Collaborator.Config =
            { NugetPacker = x.NugetPacker
              EnvironmentConfig = x.EnvironmentConfig
              WorkingDir = x.WorkingDir
              LoggerLevel = x.LoggerLevel
              LocalNugetServer = x.LocalNugetServer
              LocalNugetPackagesFolder = x.LocalNugetPackagesFolder }

    [<RequireQualifiedAccess>]
    type Target =
        | Collaborator of Collaborator.Target
        | RunCI

    type TargetStates =
        { Collaborator: Collaborator.TargetStates
          RunCI: BoxedTargetState }
    with 
        member x.Forker = x.Collaborator.Forker
        member x.NonGit = x.Forker.NonGit

    type Role =
        { Collaborator: Collaborator.Role
          TargetStates: TargetStates
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

        member x.Solution = x.Collaborator.Solution

        member x.NonGit = x.Collaborator.Forker.NonGit

        interface IRole<TargetStates>


    let create (config: Config) =
        BuildServer.install [
            AppVeyor.Installer
        ]


        { Collaborator = Collaborator.create config.AsCollaborator
          TargetStates =
            { Collaborator = Collaborator.TargetStates.init
              RunCI = TargetState.Init }

          MajorCI = BuildServer.AppVeyor
          ArtifactsName = config.ArtifactsName
        }

    [<AutoOpen>]
    module SRTPMsgs =

        type Ext = Ext
            with
                static member Bar (ext : Ext, nonGit : NonGit.Target) =
                    Collaborator.upcastMsg nonGit
                    |> Target.Collaborator

                static member Bar (ext : Ext, forker : Forker.Target) =
                    Collaborator.upcastMsg forker
                    |> Target.Collaborator

                static member Bar (ext : Ext, collaborator : Collaborator.Target) =
                    collaborator
                    |> Target.Collaborator

    let inline upcastMsg msg =
        ((^b or ^a) : (static member Bar : ^b * ^a -> Target) (Ext, msg))

    let inline private (!^) msg =
        ((^b or ^a) : (static member Bar : ^b * ^a -> Target) (Ext, msg))


    let private circleCIBuildNumber = Environment.environVar "CIRCLE_BUILD_NUM"

    let private roleAction (role: Role) = function
        | Target.Collaborator collaboratorMsg ->
            { DependsOn = []
              Action = MapChild (fun role ->
                    let newChildRole = Collaborator.run collaboratorMsg role.Collaborator
                    { role with 
                        TargetStates = 
                            { role.TargetStates with Collaborator = newChildRole.TargetStates }
                        Collaborator = newChildRole
                    }

                )
            }

        | Target.RunCI ->

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
                        let tbdReleaseNotes = ReleaseNotes.loadTbd role.ReleaseNotesFile

                        let nextVersion =
                            (tbdReleaseNotes.AssemblyVersion + "-build." + AppVeyor.Environment.BuildNumber)
                            |> SemVerInfo.parse

                        tbdReleaseNotes
                        |> ReleaseNotes.updateWithSemVerInfo nextVersion
                        |> ReleaseNotes.updateDateToToday
                        |> Some


                match nextReleaseNotes role with
                | Some nextReleaseNotes ->


                    let nugetPacker = role.Collaborator.NugetPacker

                    { DependsOn = 
                        [ 
                          !^ NonGit.Target.InstallPaketPackages
                          //!^ (NonGit.Target.AddSourceLinkPackages nugetPacker.SourceLinkCreate)
                          !^ (NonGit.Target.Test)
                          !^ (NonGit.Target.Publish (fun ops ->
                             ops
                             |> DotNet.PublishOptions.noBuild
                             |> DotNet.PublishOptions.setVersion nextReleaseNotes.SemVer
                          ))
                          !^ (Forker.Target.Pack nextReleaseNotes)
                          !^ (NonGit.Target.Zip (List.filter Project.existFullFramework role.Solution.CliProjects @ role.Solution.AspNetCoreProjects)) ]
                      Action = MapState (fun role ->
                        let appveyor = platformTool "appveyor"

                        let artifact file =
                            Workspace.exec appveyor ["PushArtifact"; file ] role.Workspace

                        Workspace.exec appveyor ["UpdateBuild"; "-Version"; SemVerInfo.normalize nextReleaseNotes.SemVer ] role.Workspace
                        
                        let zipOutputs = TargetState.getResult role.NonGit.TargetStates.Zip

                        zipOutputs |> List.iter artifact

                        let isJustAfterDraftedNewRelease = isJustAfterDraftedNewRelease role

                        let (packResult: PackResult) = role.Collaborator.Forker.TargetStates.Pack |> TargetState.getResult

                        NugetPacker.testSourceLink packResult nugetPacker

                        let newPackages = packResult.LibraryPackagePaths @ packResult.CliPackagePaths

                        newPackages |> Shell.copyFiles role.ArtifactsDirPath
                        newPackages |> List.iter artifact
                        let r = 
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
                        none

                    )}
                | None -> failwith "Pack nuget packages need last releaseNotes info. But we can't load it"

            | _ ->
                /// run tests only
                { DependsOn =
                    [!^ NonGit.Target.InstallPaketPackages; !^ NonGit.Target.Test ]

                  Action = MapState (fun _ -> none) }

    let run = 
        Role.Run roleAction