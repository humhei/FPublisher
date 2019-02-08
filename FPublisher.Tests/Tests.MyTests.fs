module Tests.MyTests
#if INTERACTIVE
#load "../.fake/build.fsx/intellisense_lazy.fsx"
#endif
open Expecto
open System.IO
open Fake.IO
open FPublisher
open System.Text.RegularExpressions
open FPublisher.Roles
open FPublisher
open FPublisher.Git
open System
open FPublisher.Nuget
open FPublisher.Roles
open FPublisher.Roles.Primitives

let pass() = Expect.isTrue true "passed"
let fail() = Expect.isTrue false "failed"

let root =  Path.getFullName (Path.Combine (__SOURCE_DIRECTORY__,"../"))


// let MyTests() =
//   let publisher = Publisher.create (fun config ->
//     { config with 
//         PublishTarget = PublishTarget.Build
//         WorkingDir = root
//         BuildingPaketGitHubServerPublisher = Some id
//         Logger = Logger.Normal }
//   )


  // testList "Interation tests" [
  //   testCase "next build" <| fun _ -> 
  //     publisher
  //     |> Publisher.publishAndDraftAll
  //     |> ignore

  //   testCase "next release" <| fun _ -> 
  //     Publisher.setPublishTarget PublishTarget.Release publisher
  //     |> Publisher.publishAndDraftAll
  //     |> ignore

  //   testCase "build project" <| fun _ -> 
  //     publisher
  //     |> Publisher.build
  //     |> ignore  

  //   testCase "run test" <| fun _ -> 
  //     Publisher.runTest publisher
  //     |> ignore

    

  // ]

let workspace = (Workspace root)  

let workspaceTests() =
  testList "Fake tests" [
    testCase "add default solution" <| fun _ ->
      Workspace.createDefaultSln false workspace 
  ]

let forker = Forker.create workspace

let forkerTests() =
  testList "Forker tests" [
    testCase "build project" <| fun _ ->
      Forker.run Forker.Msg.Build forker
      |> ignore
    
    testCase "test project" <| fun _ ->
      Forker.run Forker.Msg.Test forker
      |> ignore        
  ]
  
let collaborator = Collaborator.create (fun config ->
    { config with 
        WorkingDir = root
        LocalNugetServer = 
            Some (LocalNugetServer.BaGet ("http://localhost:5000/v3/index.json","http://localhost:5000/v3/search")) })

let collaboratorTests() =
    
  testList "Collaborator Tests" [
    ftestCase "next build" <| fun _ ->
      Collaborator.run (Collaborator.Msg.PublishAndDraftAll PublishTarget.Build) collaborator
      |> ignore
      Console.ReadLine() |> ignore
  ]