// Learn more about F# at http://fsharp.org
module Runner
open Expecto
open Expecto.Logging
open System
open Tests.MyTests
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


[<EntryPoint>]
let main argv = 
    let tests = 
        match argv with 
        | argv when Array.contains "--RunCI" argv -> allTests
        | _ -> allTests

    runTests testConfig tests

    Console.Read()
