#if FAKE
#r "paket: groupref Main //"
#endif
#if !FAKE
#r "Facades/netstandard"
#r "netstandard"
#endif
#load "./.fake/build.fsx/intellisense.fsx"
open Fake.Core
open FPublisher.Roles
open FPublisher
open FPublisher.Nuget
open FPublisher.Git
open Fake.DotNet
open System.Reflection
open System

let buildServer =
    BuildServer.create
        { BuildServer.Config.DefaultValue
            with
                LocalNugetServer = Some LocalNugetServer.DefaultValue
                LoggerLevel = Logger.Level.Normal }


Target.create "Workspace.CreateDefaultSln" <| fun _ ->
    Workspace.createDefaultSln false buildServer.Collaborator.Workspace
    |> ignore

Target.create "NonGit.Build" <| fun _ ->
    BuildServer.run (!^ NonGit.Msg.Build) buildServer
    |> ignore

Target.create "NonGit.Test" <| fun _ ->
    BuildServer.run (!^ NonGit.Msg.Test) buildServer
    |> ignore

Target.create "Forker.PublishToLocalNugetServer" <| fun _ ->
    BuildServer.run (!^ (Forker.Msg.PublishToLocalNugetServer LocalNugetServer.DefaultValue)) buildServer
    |> ignore


Target.create "Collaborator.NextRelease" <| fun _ ->
    BuildServer.run (!^ Collaborator.Msg.NextRelease) buildServer
    |> ignore

Target.create "RunCI" <| fun _ ->
    BuildServer.run (BuildServer.Msg.RunCI) buildServer
    |> ignore

Target.create "Default" ignore

Target.runOrDefault "Default"