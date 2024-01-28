// Learn more about F# at http://fsharp.org

open System
open Argu
open FPublisher.Roles
open FPublisher
open FPublisher.Solution
open System.IO
open FPublisher.Nuget
open FPublisher.Nuget.Nuget
open Fake.Core

type Arguments =
    | WorkingDir of string
    | Create_Sln
    | Clean
    | Build
    | Test
    | Next_Release
    | Run_CI
    | Baget
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | WorkingDir _ -> "specific working dir" 
            | Create_Sln -> "create sln if not exists"
            | Clean -> "clean bin and obj"
            | Build -> "build solution"
            | Test _ -> "test projects"
            | Next_Release -> "draft next release"
            | Run_CI -> "only invoked by CI"
            | Baget -> "publish packages to local baget nuget server; port is read from environment baget_port; defalut port is 5000"

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<Arguments>(programName = "fpublisher.exe")
    let usage = parser.Parse argv
    let createLocalNugetServer port =
      {ApiEnvironmentName = None; Serviceable = sprintf "http://127.0.0.1:%d/v3/index.json" port; SearchQueryService = sprintf "http://127.0.0.1:%d/v3/search" port}
        
    let allResults = usage.GetAllResults()
    let workingDir = 
        allResults
        |> List.tryPick(fun m ->
            match m with 
            | Arguments.WorkingDir dir ->   
                printf "Origin Working Dir %s" (Directory.GetCurrentDirectory())
                let newWorkingDir = Path.GetFullPath dir
                printf "Specific Working Dir %s" (newWorkingDir)
                Some (newWorkingDir)
            | _ -> None
        )

    let allResults =
        allResults
        |> List.filter(fun m ->
            match m with 
            | Arguments.WorkingDir _ -> false
            | _ -> true
        )

    match allResults with 
    | [result] ->
        let execContext = Fake.Core.Context.FakeExecutionContext.Create false "generate.fsx" []
        Fake.Core.Context.setExecutionContext (Fake.Core.Context.RuntimeContext.Fake execContext)
        
        let localNugetServer =
            match Environment.environVarOrNone "baget_port" with 
            | Some port -> createLocalNugetServer (System.Int32.Parse port)
                
             
            | None -> createLocalNugetServer 4000
            |> Some 

        let buildServer =
            BuildServer.create
                { BuildServer.Config.DefaultValue
                    with
                        LoggerLevel = Logger.Level.Normal
                        LocalNugetServer = localNugetServer
                        WorkingDir = defaultArg (workingDir) (System.IO.Directory.GetCurrentDirectory())
                }


        match result with 
        | Create_Sln -> Workspace.createDefaultSln false buildServer.Workspace
        | Clean -> Workspace.cleanBinAndObj buildServer.Workspace
        | Build -> BuildServer.run (!^ (NonGit.Target.Build id)) buildServer |> ignore
        | Test -> BuildServer.run (!^ NonGit.Target.Test) buildServer |> ignore
        | Next_Release -> BuildServer.run (!^ Collaborator.Target.NextRelease) buildServer |> ignore
        | Run_CI -> BuildServer.run (BuildServer.Target.RunCI) buildServer |> ignore
        | Baget -> BuildServer.run (!^ Forker.Target.PublishToLocalNugetServer) buildServer |> ignore
        | WorkingDir _ -> failwithf "Invalid token"
    | _ -> 
        parser.PrintUsage()
        |> printfn "%s"

    0 // return an integer exit code
