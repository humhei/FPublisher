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


let workspace = (Workspace root)  


let workspaceTests() =
  testList "Fake tests" [
    testCase "add default solution" <| fun _ ->
      Workspace.createDefaultSln false workspace 
  ]

let collaborator = 
    Collaborator.create 
        { Collaborator.Config.DefaultValue 
            with 
                WorkingDir = root
                LoggerLevel = Logger.Level.Normal
                LocalNugetServer = Some LocalNugetServer.DefaultValue }


let nonGitTests() =
  testList "NonGit tests" [
    testCase "build project" <| fun _ ->
      Collaborator.run (!^ NonGit.Msg.Build) collaborator
      |> ignore
    testCase "test project" <| fun _ ->
      Collaborator.run (!^ NonGit.Msg.Test) collaborator
      |> ignore     
  ]
  


let forkerTests() =
  testList "forker tests" [
    testCase "pbulish to local nuget server" <| fun _ ->
      Collaborator.run (!^ (Forker.Msg.PublishToLocalNugetServer LocalNugetServer.DefaultValue)) collaborator
      |> ignore
  ]


let collaboratorTests() =
    
  testList "Collaborator Tests" [
    ftestCase "next build" <| fun _ ->
      Collaborator.run Collaborator.Msg.NextRelease collaborator
      |> ignore
  ]