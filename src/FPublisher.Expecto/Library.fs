namespace FPublisher
open Expecto
open System
open System.IO
open System.Reflection

[<RequireQualifiedAccess>]
module ExpectoConfig =
    let appendSummaryHandler argv (testConfig: Impl.ExpectoConfig) =
        argv
        |> Array.tryFind (fun arg ->
            String.Compare(arg,"--summary",true) = 0
        )
        |> function
            | Some _ ->
                let assemblyName =
                    Path.GetFileNameWithoutExtension(Assembly.GetEntryAssembly().CodeBase)

                let xmlFile = assemblyName + ".TestResults.xml"

                let writeResults = TestResults.writeNUnitSummary (xmlFile, assemblyName)
                testConfig.appendSummaryHandler writeResults
            | None -> testConfig