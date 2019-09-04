namespace FPublisher.Roles
open FPublisher
open Fake.IO.FileSystemOperators
open Fake.Core
open Fake.DotNet
open System.IO
open Fake.IO
open Primitives
open Solution
open FPublisher.Nuget
open Fake.IO.Globbing.Operators
open Fake.DotNet.NuGet.NuGet

[<RequireQualifiedAccess>]
module NonGit =

    [<RequireQualifiedAccess>]
    type Target =
        | InstallPaketPackages
        | Build of (DotNet.BuildOptions -> DotNet.BuildOptions)
        | Pack of (FPublisher.DotNet.PackOptions -> FPublisher.DotNet.PackOptions)
        | Test
        | PushToLocalNugetServerV3
        | Publish of (DotNet.PublishOptions -> DotNet.PublishOptions)


    type TargetStates =
        { InstallPaketPackages: BoxedTargetState
          Build: BoxedTargetState
          Pack: TargetState<list<ProjectKind * Project list>>
          Test: BoxedTargetState
          PushToLocalNugetServerV3: BoxedTargetState
          Publish: BoxedTargetState }

    [<RequireQualifiedAccess>]
    module TargetStates =
        let init =
            { InstallPaketPackages = TargetState.Init
              Build = TargetState.Init
              Pack = TargetState.Init
              Test = TargetState.Init
              PushToLocalNugetServerV3 = TargetState.Init
              Publish = TargetState.Init }

    type Role =
        { Solution: Solution
          Workspace: Workspace
          LocalNugetServerV3: NugetServer option
          TargetStates: TargetStates }
    with 
        interface IRole<TargetStates>


    let create loggerLevel localNugetServerV3 (workspace: Workspace) =
        logger <- Logger.create(loggerLevel)
        let slnPath =
            let defaultSlnName =
                match Workspace.tryGetRepoName workspace with
                | Some repoName -> repoName
                | None -> Path.GetFileName workspace.WorkingDir


            let poiorSlnPath = 
                let priorSlnName = defaultSlnName + ".FPublisher"

                workspace.WorkingDir </> (priorSlnName  + ".sln")

            if File.Exists poiorSlnPath 
            then poiorSlnPath
            else workspace.WorkingDir </> (defaultSlnName  + ".sln")

        { Solution = Solution.read slnPath
          Workspace = workspace
          LocalNugetServerV3 = localNugetServerV3
          TargetStates = TargetStates.init }

    let run =
        IRole.Run (fun role target ->
            match target with
            | Target.InstallPaketPackages ->
                { DependsOn = [] 
                  Action = MapState (fun role -> Workspace.paket ["install"] role.Workspace; none)}

            | Target.Build setParams ->
                { DependsOn = [Target.InstallPaketPackages] 
                  Action = MapState (fun role -> Solution.build setParams role.Solution ; none)}

            | Target.Pack setParams ->
                { DependsOn = [Target.InstallPaketPackages] 
                  Action = MapState (fun role -> Solution.pack setParams role.Solution |> box)}

            | Target.PushToLocalNugetServerV3 ->
                match role.LocalNugetServerV3 with 
                | Some localNugetServer ->
                    let outputDirectory = Path.GetTempPath() </> Path.GetRandomFileName()

                    Directory.ensure outputDirectory

                    let packTarget = 
                        let version = 
                            let packageNames = Solution.getPackageNames role.Solution

                            let lastVersions = 
                                packageNames
                                |> List.choose (fun packageName ->
                                    NugetServer.getLastNugetVersionV3 packageName true localNugetServer
                                )
                                |> List.map (SemVer.parse)

                            if lastVersions.IsEmpty then "1.0.0"
                            else 
                                let maxVersion  = (List.max lastVersions)
                                { maxVersion with Patch = maxVersion.Patch + 1u }.Normalize()

                        Target.Pack (fun ops ->
                            { ops with 
                                OutputPath = Some outputDirectory 
                                Configuration = DotNet.BuildConfiguration.Release 
                                Version = Some version }
                        )

                    { DependsOn = [Target.InstallPaketPackages; packTarget] 
                      Action = MapState (fun role -> 
                        let nupkgs = !! (outputDirectory </> "./*.nupkg")
                        for nupkg in nupkgs do
                            DotNet.nugetPush (fun ops -> 
                                {ops with 
                                    PushParams = 
                                        { NuGetPushParams.Create() with     
                                            ApiKey = localNugetServer.ApiEnvironmentName 
                                            Source = Some localNugetServer.Serviceable
                                        }
                                }
                            ) nupkg
                        none
                      )}

                | None -> 
                    logger.Warn "role %A doesn't include a local nuget server" role
                    { DependsOn = [] 
                      Action = MapState (fun role -> none) }

            | _ -> failwith "Not implemented"
        )