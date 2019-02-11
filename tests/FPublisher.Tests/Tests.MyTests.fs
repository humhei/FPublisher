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
  testList "Fake tests" [
    testCase "add default solution" <| fun _ ->
      Workspace.createDefaultSln false workspace
  ]

let role =
    BuildServer.create BuildServer.buildServer
        { Collaborator.Config.DefaultValue
            with
                WorkingDir = root
                LoggerLevel = Logger.Level.Normal
                LocalNugetServer = Some LocalNugetServer.DefaultValue }


let nonGitTests() =
  testList "NonGit tests" [
    testCase "build project" <| fun _ ->
      BuildServer.run (!^ NonGit.Msg.Build) role
      |> ignore
    testCase "test project" <| fun _ ->
      BuildServer.run (!^ NonGit.Msg.Test) role
      |> ignore
  ]



let forkerTests() =
  testList "forker tests" [
    testCase "pbulish to local nuget server" <| fun _ ->
      BuildServer.run (!^ (Forker.Msg.PublishToLocalNugetServer LocalNugetServer.DefaultValue)) role
      |> ignore
  ]


let collaboratorTests() =
  testList "Collaborator Tests" [
    testCase "next release" <| fun _ ->
      BuildServer.run (!^ Collaborator.Msg.NextRelease) role
      |> ignore
  ]

