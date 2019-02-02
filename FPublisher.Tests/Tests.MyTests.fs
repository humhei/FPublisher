module Tests.MyTests
#if INTERACTIVE
#load "../.fake/build.fsx/intellisense_lazy.fsx"
#endif
open Expecto
open FPublisher.FPublisher
open System.IO
open Fake.IO
open FPublisher.Utils
open System.Text.RegularExpressions

let pass() = Expect.isTrue true "passed"
let fail() = Expect.isTrue false "failed"

let root =  Path.getFullName (Path.Combine (__SOURCE_DIRECTORY__,"../"))
Logger.info "Begin create publisher" 
let publisher = Publisher.create (fun config ->
  { config with 
      PublishTarget = PublishTarget.Build
      WorkingDir = root
      BuildingPaketGitHubServerPublisher = Some id
      Logger = Logger.Normal }
)
Logger.info "End create publisher" 

let MyTests =
  testList "Interation tests" [
    testCase "next build" <| fun _ -> 
      publisher
      |> Publisher.publishAndDraftAll
      |> ignore

    testCase "next release" <| fun _ -> 
      Publisher.setPublishTarget PublishTarget.Release publisher
      |> Publisher.publishAndDraftAll
      |> ignore

    testCase "build project" <| fun _ -> 
      publisher
      |> Publisher.build
      |> ignore  

    testCase "run test" <| fun _ -> 
      Publisher.runTest publisher
      |> ignore

    

  ]