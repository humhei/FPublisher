module Tests.MyTests
#if INTERACTIVE
#load "../../.fake/build.fsx/intellisense_lazy.fsx"
#endif
open Expecto
open System.IO
open Fake.IO
open FPublisher
open FPublisher.Roles
open FPublisher.Git
open FPublisher.Nuget
open Fake.Core


let pass() = Expect.isTrue true "passed"
let fail() = Expect.isTrue false "failed"

let root =  Path.getFullName (Path.Combine (__SOURCE_DIRECTORY__,"../../"))

#if !FAKE
let execContext = Fake.Core.Context.FakeExecutionContext.Create false "generate.fsx" [] 
Fake.Core.Context.setExecutionContext (Fake.Core.Context.RuntimeContext.Fake execContext)
#endif

let workspace = (Workspace root)

let workspaceTests() =
  ptestList "Workspace tests" [
    testCase "add default solution" <| fun _ ->
      Workspace.createDefaultSln false workspace
  ]


let role =
    BuildServer.create
        { BuildServer.Config.DefaultValue
            with
                WorkingDir = root
                LoggerLevel = Logger.Level.Normal
                LocalNugetServer = Some NugetServer.DefaultBaGetLocal }


let nonGitTests() =
  testList "NonGit tests" [
    testCase "build project" <| fun _ ->
      BuildServer.run (!^ (NonGit.Msg.Build None)) role

    testCase "test project" <| fun _ ->
      BuildServer.run (!^ NonGit.Msg.Test) role
  ]



let forkerTests() =
  ftestList "forker tests" [
    testCase "publish to local nuget server" <| fun _ ->
      BuildServer.run (!^ (Forker.Msg.PublishToLocalNugetServer)) role
  ]

let collaboratorTests() =
  testList "Collaborator Tests" [
    testCase "next release" <| fun _ ->
      BuildServer.run (!^ Collaborator.Msg.NextRelease) role
  ]

let buildServerTests() =
  testList "BuildServer Tests" [
    testCase "RunCI" <| fun _ ->
      BuildServer.run (BuildServer.Msg.RunCI) role
  ]
