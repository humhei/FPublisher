// Learn more about F# at http://fsharp.org

open System
open Argu
open FPublisher.Roles
open FPublisher

type Arguments =
    | Create_Sln
    | Clean
    | Build
    | Test
    | Next_Release
    | Run_CI
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

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<Arguments>(programName = "fpublisher.exe")
    let usage = parser.Parse argv

    match usage.GetAllResults() with 
    | [result] ->
        let execContext = Fake.Core.Context.FakeExecutionContext.Create false "generate.fsx" []
        Fake.Core.Context.setExecutionContext (Fake.Core.Context.RuntimeContext.Fake execContext)
        
        let buildServer =
            BuildServer.create
                { BuildServer.Config.DefaultValue
                    with
                        LoggerLevel = Logger.Level.Normal }


        match result with 
        | Create_Sln -> Workspace.createDefaultSln false buildServer.Workspace
        | Clean -> Workspace.cleanBinAndObj buildServer.Workspace
        | Build -> BuildServer.run (!^ (NonGit.Msg.Build None)) buildServer
        | Test -> BuildServer.run (!^ NonGit.Msg.Test) buildServer
        | Next_Release -> BuildServer.run (!^ Collaborator.Msg.NextRelease) buildServer
        | Run_CI -> BuildServer.run (BuildServer.Msg.RunCI) buildServer
    | _ -> 
        parser.PrintUsage()
        |> printfn "%s"

    0 // return an integer exit code
