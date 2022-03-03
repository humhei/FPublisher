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
        | Clean
        | Build of (DotNet.BuildOptions -> DotNet.BuildOptions)
        | Pack of (FPublisher.DotNet.PackOptions -> FPublisher.DotNet.PackOptions)
        | Test
        | PushToLocalNugetServerV3
        | PushToLocalPackagesFolder
        | Publish of (DotNet.PublishOptions -> DotNet.PublishOptions)


    type TargetStates =
        { InstallPaketPackages: BoxedTargetState
          Clean: BoxedTargetState
          Build: BoxedTargetState
          Pack: TargetState<list<ProjectKind * Project list>>
          Test: BoxedTargetState
          PushToLocalNugetServerV3: BoxedTargetState
          PushToLocalPackagesFolder: BoxedTargetState
          Publish: BoxedTargetState }

    [<RequireQualifiedAccess>]
    module TargetStates =
        let init =
            { InstallPaketPackages = TargetState.Init
              Clean = TargetState.Init
              Build = TargetState.Init
              Pack = TargetState.Init
              Test = TargetState.Init
              PushToLocalNugetServerV3 = TargetState.Init
              PushToLocalPackagesFolder = TargetState.Init
              Publish = TargetState.Init }

    type Role =
        { Solution: Solution
          Workspace: Workspace
          LocalNugetServerV3: NugetServer option
          LocalPackagesFolder: string option
          TargetStates: TargetStates }
    with 
        interface IRole<TargetStates>


    let create loggerLevel localNugetServerV3 localPackagesFolder (workspace: Workspace) =
        logger <- Logger.create(loggerLevel)

        let repoName = Workspace.tryGetRepoName workspace


        let poiorSlnPath1 = 
            match repoName with 
            | Some repoName -> 
                 workspace.WorkingDir </> (repoName  + ".FPublisher" + ".sln")
                 |> Some
            | None -> None
           
        let poiorSlnPath2 = 
            match repoName with 
            | Some repoName -> 
                 workspace.WorkingDir </> (repoName + ".sln")
                 |> Some
            | None -> None

        let poiorSlnPath3 =  
            workspace.WorkingDir </> (Path.GetFileName workspace.WorkingDir + ".FPublisher" + ".sln")
            |> Some

        let poiorSlnPath4 =  
            workspace.WorkingDir </> (Path.GetFileName workspace.WorkingDir + ".sln")
            |> Some

        let slnPaths = 
            [
                poiorSlnPath1
                poiorSlnPath2
                poiorSlnPath3
                poiorSlnPath4
            ]
            |> List.choose id

        match List.tryFind (File.exists) slnPaths with 
        | Some slnPath ->

            { Solution = Solution.read slnPath
              Workspace = workspace
              LocalNugetServerV3 = localNugetServerV3
              LocalPackagesFolder = localPackagesFolder
              TargetStates = TargetStates.init }

        | None -> failwithf "Solution of %A not found" slnPaths

    let run =
        IRole.Run (fun role target ->
            match target with
            | Target.InstallPaketPackages ->
                { DependsOn = [] 
                  Action = MapState (fun role -> Workspace.paket ["install"] role.Workspace; none)}

            | Target.Clean ->
                { DependsOn = [] 
                  Action = MapState (fun role -> Solution.clean role.Solution; none)}


            | Target.Build setParams ->
                { DependsOn = [Target.InstallPaketPackages] 
                  Action = MapState (fun role -> Solution.build setParams role.Solution ; none)}

            | Target.Pack setParams ->
                { DependsOn = [Target.InstallPaketPackages] 
                  Action = MapState (fun role -> Solution.pack setParams role.Solution |> box)}

            | Target.Test ->
                failwith "Not implemented"

            | Target.PushToLocalNugetServerV3 ->
                match role.LocalNugetServerV3 with 
                | Some localNugetServer ->
                    let role =
                        { role with 
                            Solution = 
                                Solution.updatePackages localNugetServer role.Solution
                            }
                        
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

                            if lastVersions.IsEmpty then "0.0.1"
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

                        match role.LocalPackagesFolder with 
                        | Some localPackagesFolder -> 
                            let nugetTool =
                                let platformTool tool =
                                    ProcessUtils.tryFindFileOnPath tool
                                    |> function Some t -> t | _ -> failwithf "%s not found" tool
                                platformTool "nuget"

                            let run cmd dir args =
                                let result =
                                    CreateProcess.fromRawCommandLine cmd args
                                    |> CreateProcess.withWorkingDirectory dir
                                    |> Proc.run
                                if result.ExitCode <> 0 then
                                    failwithf "Error while running '%s' with args: %s " cmd args

                            nupkgs
                            |> Seq.iter(fun nupkg ->
                                let destFile = localPackagesFolder </> (Path.GetFileName nupkg)
                                run nugetTool role.Workspace.WorkingDir (sprintf "add %s -source %s" nupkg localPackagesFolder) 

                                //File.Copy(nupkg, destFile, true)
                            )

                        | None -> ()

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


            | Target.PushToLocalPackagesFolder -> failwith "Not implemented"
                

            | Target.Publish _ -> failwith "Not implemented"
        )