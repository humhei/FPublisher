module Tests.MyTests
#if INTERACTIVE
#load "../../.fake/build.fsx/intellisense_lazy.fsx"
#endif
open Expecto
open System.IO
open Fake.IO
open Fake.IO.Globbing.Operators
open FPublisher
open FPublisher.Roles
open Fake.Core
open FPublisher.Nuget
open FPublisher.Solution
open System.Text.RegularExpressions
open FParsec
open Shrimp.FSharp.Plus
open Deedle
open FSharp.Linq.RuntimeHelpers
open FPublisher.Nuget.Nuget
open FPublisher.FakeHelper.Build


let pass() = Expect.isTrue true "passed"
let fail() = Expect.isTrue false "failed"


let root =  Path.getFullName (Path.Combine (__SOURCE_DIRECTORY__,"../../"))


#if !FAKE
let execContext = Fake.Core.Context.FakeExecutionContext.Create false "generate.fsx" []
Fake.Core.Context.setExecutionContext (Fake.Core.Context.RuntimeContext.Fake execContext)
#endif


let workspace = (Workspace root)

//let workspaceTests() =
//  testList "Workspace tests" [
//    testCase "add default solution" <| fun _ ->
//      Workspace.createDefaultSln false workspace

//    testCase "clean bin and obj" <| fun _ ->
//      Workspace.cleanBinAndObj workspace
//  ]




//let nonGitTests() =
  
//  let role = NonGit.create Logger.Level.Normal (Some {ApiEnvironmentName = None; Serviceable = "http://127.0.0.1:4000/v3/index.json"; SearchQueryService = "http://127.0.0.1:4000/v3/search"}) localPackagesFolder workspace
//  let localNugetServer: NugetServer =
//    {ApiEnvironmentName = None; Serviceable = "http://127.0.0.1:4000/v3/index.json"; SearchQueryService = "http://127.0.0.1:4000/v3/search"}
//  testList "NonGit tests" [
//    testCase "paket install" <| fun _ ->
//      NonGit.run (NonGit.Target.InstallPaketPackages) role
//      |> ignore

//    testCase "build projects" <| fun _ ->
//      NonGit.run (NonGit.Target.Build id) role
//      |> ignore

//    testCase "pack projects" <| fun _ ->
//      NonGit.run (NonGit.Target.Pack id) role
//      |> ignore

//    testCase "test projects" <| fun _ ->                      
//      NonGit.run (NonGit.Target.Test) role              
//      |> ignore
//  ]                                                   
                                           
              
   


//let collaboratorTests() =
//  testList "Collaborator Tests" [
//    testCase "next release" <| fun _ ->
//      BuildServer.run (!^ Collaborator.Msg.NextRelease) role
//  ]

let buildServerTests() =
  testList "BuildServer Tests" [
    testCase "RunCI" <| fun _ ->
      BuildServer.run (BuildServer.Target.RunCI) role
      |> ignore
  ]
                             
                              
                                                        








