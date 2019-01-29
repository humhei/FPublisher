module Tests.MyTests
#if INTERACTIVE
#load "../.fake/build.fsx/intellisense_lazy.fsx"
#endif
open Expecto
open FPublisher.FPublisher
open System.IO
open Fake.IO

let pass() = Expect.isTrue true "passed"
let fail() = Expect.isTrue false "failed"

let root =  Path.getFullName (Path.Combine (__SOURCE_DIRECTORY__,"../"))
let publisher = Publisher.create (fun config ->
  { config with 
      PublishTarget = PublishTarget.Build
      WorkingDir = root
      BuildingPaketGitHubServerPublisher = Some id }
)

let MyTests =
  testList "Interation tests" [
    testCase "Publish and draft all" <| fun _ -> 
      Publisher.publishAndDraftAll publisher
  ]