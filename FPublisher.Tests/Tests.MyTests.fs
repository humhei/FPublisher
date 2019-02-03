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
open FPublisher.Roles
open FPublisher

let pass() = Expect.isTrue true "passed"
let fail() = Expect.isTrue false "failed"

let root =  Path.getFullName (Path.Combine (__SOURCE_DIRECTORY__,"../"))


let MyTests() =
  let publisher = Publisher.create (fun config ->
    { config with 
        PublishTarget = PublishTarget.Build
        WorkingDir = root
        BuildingPaketGitHubServerPublisher = Some id
        Logger = Logger.Normal }
  )


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

let forker = Forker.create (Workspace root)

let forkerTests() =
  testList "Forker tests" [
    testCase "build project" <| fun _ ->
      Forker.run Forker.Msg.Build forker
      |> ignore
    ftestCase "test project" <| fun _ ->
      Forker.run Forker.Msg.Test forker
      |> ignore        
  ]