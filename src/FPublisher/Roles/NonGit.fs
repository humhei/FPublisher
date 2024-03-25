namespace FPublisher.Roles
open FPublisher
open Fake.IO.FileSystemOperators
open Fake.Core
open Fake.DotNet
open System.IO
open Fake.IO
open Primitives
open FPublisher.Solution
open FPublisher.Nuget
open Fake.IO.Globbing.Operators
open Fake.DotNet.NuGet.NuGet
open FPublisher.Nuget.Nuget
open FPublisher.Nuget.NugetPacker
open FPublisher.FakeHelper.CommandHelper

[<RequireQualifiedAccess>]
module NonGit =

    [<RequireQualifiedAccess>]
    type Target =
        | InstallPaketPackages
        | Clean
        | Build_Debug of (DotNet.BuildOptions -> DotNet.BuildOptions)
        | Build_Release of (DotNet.BuildOptions -> DotNet.BuildOptions)
        | Pack of (FPublisher.DotNet.PackOptions -> FPublisher.DotNet.PackOptions)
        | AddSourceLinkPackages of SourceLinkCreate
        | Test
        | PushToLocalNugetServerV3
        | PushToLocalPackagesFolder
        | Publish of (DotNet.PublishOptions -> DotNet.PublishOptions)
        | Zip of Project list


    type TargetStates =
        { InstallPaketPackages: BoxedTargetState
          Clean: BoxedTargetState
          Build_Debug: BoxedTargetState
          Build_Release: BoxedTargetState
          Pack: BoxedTargetState
          AddSourceLinkPackages: BoxedTargetState
          Test: BoxedTargetState
          PushToLocalNugetServerV3: BoxedTargetState
          PushToLocalPackagesFolder: BoxedTargetState
          Publish: BoxedTargetState
          Zip: BoxedTargetState }

    [<RequireQualifiedAccess>]
    module TargetStates =
        let init =
            { InstallPaketPackages = TargetState.Init
              Clean = TargetState.Init
              Build_Debug = TargetState.Init
              Build_Release = TargetState.Init
              Pack = TargetState.Init
              AddSourceLinkPackages = TargetState.Init
              Test = TargetState.Init
              PushToLocalNugetServerV3 = TargetState.Init
              PushToLocalPackagesFolder = TargetState.Init
              Publish = TargetState.Init
              Zip = TargetState.Init }

    type Role =
        { Solution: Solution
          Workspace: Workspace
          LocalNugetServerV3: NugetServer option
          LocalPackagesFolder: string option
          TargetStates: TargetStates }
    with 
        interface IRole<TargetStates>

    let mutable private isDotnetInfoExecuted = false

    let create loggerLevel localNugetServerV3 localPackagesFolder (workspace: Workspace) =
        match isDotnetInfoExecuted with 
        | false -> 
            dotnet "--info" [] (workspace.WorkingDir)
            isDotnetInfoExecuted <- true
        | true -> ()

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


            | Target.Build_Debug setParams ->   
                let setParams (ops: DotNet.BuildOptions) =
                    let ops = setParams ops
                    { ops with 
                        Configuration = DotNet.BuildConfiguration.Debug
                        MSBuildParams = 
                            { ops.MSBuildParams with 
                                DisableInternalBinLog = true }
                        }
                { DependsOn = [Target.InstallPaketPackages] 
                  Action = MapState (fun role -> Solution.build setParams role.Solution ; none)}

            | Target.Build_Release setParams ->  
                let setParams (ops: DotNet.BuildOptions) =
                    let ops = setParams ops
                    { ops with 
                        Configuration = DotNet.BuildConfiguration.Release
                        MSBuildParams = 
                            { ops.MSBuildParams with DisableInternalBinLog = true }
                        }
                { DependsOn = [Target.InstallPaketPackages] 
                  Action = MapState (fun role -> Solution.build setParams role.Solution ; none)}

            | Target.Pack setParams ->
                let buildOps =
                    let ops = setParams FPublisher.DotNet.PackOptions.DefaultValue
                    DotNet.PackOptions.asFakeBuildOptions ops

                { DependsOn = [Target.InstallPaketPackages] 
                  Action = MapState (fun role -> Solution.pack setParams role.Solution |> box)}

            | Target.AddSourceLinkPackages sourceLinkCreate  ->
                { DependsOn = [] 
                  Action = MapState (fun role ->
                    let solution = role.Solution
                    SourceLinkCreate.addSourceLinkPackages solution sourceLinkCreate
                    none
                )}

            | Target.Test ->
                { DependsOn = [ Target.Build_Release id ]
                  Action = MapState (fun role -> Solution.test DotNet.BuildConfiguration.Release role.Solution; none) }

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
                        let packResult: PackResult = TargetState.getResult role.TargetStates.Pack
                        let nupkgs = packResult.LibraryPackagePaths @ packResult.CliPackagePaths

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

                        logger.ImportantGreen "publishing nupkgs to localNugetServer %s" localNugetServer.SearchQueryService

                        NugetServer.publish (nupkgs) localNugetServer |> Async.RunSynchronously
                        none

                      )}

                | None -> 
                    logger.Warn "role %A doesn't include a local nuget server" role
                    { DependsOn = [] 
                      Action = MapState (fun role -> none) }


            | Target.PushToLocalPackagesFolder -> failwith "Not implemented"

            | Target.Publish setParams -> 
                let setParams ops = setParams ({ ops with Configuration = DotNet.BuildConfiguration.Release })
                { DependsOn = [ Target.Build_Release id ]
                  Action = MapState (fun role -> 
                    Solution.publish PublishNetCoreDependency.Keep setParams role.Solution
                    none
                  ) }

            | Target.Zip projects ->

                { DependsOn = [ Target.Build_Release id ]
                  Action = MapState (fun role -> 
                    projects 
                    |> List.collect (fun project ->
                        project.GetOutputPaths(DotNet.BuildOptions.Create().Configuration) |> List.map (fun outputPath ->
                            let dir = Path.getDirectory outputPath
                            let zipPath = 
                                let randomDir = 
                                    Path.GetTempPath() </> Path.GetRandomFileName()

                                Directory.ensure randomDir

                                randomDir </> project.Name + ".zip"

                            !! (dir </> "**")
                            |> List.ofSeq
                            |> Zip.zip dir zipPath
                            zipPath
                        ) 
                    )
                    |> box 
                ) }

        )