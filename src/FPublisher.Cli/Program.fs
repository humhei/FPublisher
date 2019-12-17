// Learn more about F# at http://fsharp.org

open System
open Argu
open FPublisher.Roles
open FPublisher
open FPublisher.Nuget
open Fake.Core
open System.IO

type Arguments =
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

    match usage.GetAllResults() with 
    | [result] ->
        let execContext = Fake.Core.Context.FakeExecutionContext.Create false "generate.fsx" []
        Fake.Core.Context.setExecutionContext (Fake.Core.Context.RuntimeContext.Fake execContext)
        

        match result with 
        //| Create_Sln -> Workspace.createDefaultSln false buildServer.Workspace
        //| Clean -> Workspace.cleanBinAndObj buildServer.Workspace
        //| Build -> BuildServer.run (!^ (NonGit.Msg.Build None)) buildServer
        //| Test -> BuildServer.run (!^ NonGit.Msg.Test) buildServer
        //| Next_Release -> BuildServer.run (!^ Collaborator.Msg.NextRelease) buildServer
        //| Run_CI -> BuildServer.run (BuildServer.Msg.RunCI) buildServer
        | Baget -> 
            let workspace = Workspace (Directory.GetCurrentDirectory())
            let role = NonGit.create Logger.Level.Normal (Some {ApiEnvironmentName = None; Serviceable = "http://127.0.0.1:4000/v3/index.json"; SearchQueryService = "http://127.0.0.1:4000/v3/search"}) workspace
            NonGit.run NonGit.Target.PushToLocalNugetServerV3 role
            |> ignore
        | _ -> failwith "Not impelment"
    | _ -> 
        parser.PrintUsage()
        |> printfn "%s"

    0 // return an integer exit code
