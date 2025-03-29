
// Learn more about F# at http://fsharp.org
module Runner
open Expecto
open Expecto.Logging
open FPublisher
open Fake.Core

let testConfig =
    { Expecto.Tests.defaultConfig with
         parallelWorkers = 1
         verbosity = LogLevel.Debug }
            

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
    let tests = Tests.LocalTests.localTests()

    runTests testConfig tests                              

    0
