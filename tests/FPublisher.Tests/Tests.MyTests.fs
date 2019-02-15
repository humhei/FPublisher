module Tests.MyTests
#if INTERACTIVE
#load "../../.fake/build.fsx/intellisense_lazy.fsx"
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
open Fake.Core

let pass() = Expect.isTrue true "passed"
let fail() = Expect.isTrue false "failed"

let root =  Path.getFullName (Path.Combine (__SOURCE_DIRECTORY__,"../../"))

let workspace = (Workspace root)


let workspaceTests() =
  ptestList "Fake tests" [
    testCase "add default solution" <| fun _ ->
      Workspace.createDefaultSln false workspace
  ]

let role =
    BuildServer.create
        { BuildServer.Config.DefaultValue
            with
                WorkingDir = root
                LoggerLevel = Logger.Level.Normal
                LocalNugetServer = Some LocalNugetServer.DefaultValue }


let nonGitTests() =
  ptestList "NonGit tests" [
    testCase "build project" <| fun _ ->
      BuildServer.run (!^ NonGit.Msg.Build) role
      |> ignore
    ftestCase "test project" <| fun _ ->
      BuildServer.run (!^ NonGit.Msg.Test) role
      |> ignore
  ]



let forkerTests() =
  testList "forker tests" [
    testCase "publish to local nuget server" <| fun _ ->
      BuildServer.run (!^ (Forker.Msg.PublishToLocalNugetServer LocalNugetServer.DefaultValue)) role
      |> ignore
  ]


let collaboratorTests() =
  ptestList "Collaborator Tests" [
    testCase "next release" <| fun _ ->
      BuildServer.run (!^ Collaborator.Msg.NextRelease) role
      |> ignore
  ]
