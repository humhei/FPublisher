
// Learn more about F# at http://fsharp.org
module Runner
open Expecto
open Expecto.Logging
open System
open Tests.MyTests
open Fake.IO
open FPublisher
open Fake.Core

let testConfig =
    { Expecto.Tests.defaultConfig with
         parallelWorkers = 1
         verbosity = LogLevel.Debug }
            
let allTests =
    testList "All tests" [
        nonGitTests()
        //forkerTests()
        //workspaceTests()
        //collaboratorTests()
    ]


let private exec tool args (Workspace dir) =
    args
    |> CreateProcess.fromRawCommand tool
    |> CreateProcess.withWorkingDirectory dir
    |> Proc.run

let private platformTool tool =
    ProcessUtils.tryFindFileOnPath tool
    |> function Some t -> t | _ -> failwithf "%s not found" tool


[<EntryPoint>]
let main argv = 
    let tests = 
        match argv with 
        | argv when Array.contains "--RunCI" argv -> allTests
        | _ -> allTests

    //let json0 = @"D:\Users\Jia\Documents\MyData\Docs\2017\建乐\简单订单产品\卡片\.operate\卡片.json"
    //let json = "\"" + json0 + "\""
    //let workingDir = @"D:\Users\Jia\Documents\MyData\Docs\2017\建乐\简单订单产品\卡片\"

    //match (exec (platformTool "ws") ["--operate"; json0] (Workspace workingDir)).ExitCode with 
    //| 0 -> 
    //    //let binFile = BinaryFile(product.OutputBinary)
    //    "Success"
    //    //BinaryFile.deserialize<CellScript.Core.Table>(binFile)

    //| i -> failwith "Error while running ws.exe" 
        


    runTests testConfig tests

    Console.Read()
