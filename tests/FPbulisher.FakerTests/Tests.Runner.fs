// Learn more about F# at http://fsharp.org
module Runner
open Expecto
open Expecto.Logging
open System
open Tests.MyTests
open System.Reflection
open System.IO
open FPublisher
let testConfig =
    { Expecto.Tests.defaultConfig with
         parallelWorkers = 1
         verbosity = LogLevel.Debug }

let allTests =
    testList "All tests" [
        MyTests
    ]


[<EntryPoint>]
let main argv =
    runTests (ExpectoConfig.appendSummaryHandler argv testConfig) allTests
