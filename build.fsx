#if FAKE
#r "paket: groupref Main //"
#endif
#if !FAKE
#r "Facades/netstandard"
#r "netstandard"
#endif
#load "./.fake/build.fsx/intellisense.fsx"

open Fake.DotNet.NuGet
open Fake.Core
open FParsec
open Fake.IO
open System.Text
open FParsec.CharParsers
open Fake.Core.SemVerActivePattern
open System.Text.RegularExpressions
open FSharp.Data
open FPublisher.Roles
open FPublisher
open FPublisher.Nuget
open System


let collaborator = 
    Collaborator.create 
        { Collaborator.Config.DefaultValue 
            with 
                LocalNugetServer = Some LocalNugetServer.DefaultValue }


Target.create "Build" <| fun _ ->
    Collaborator.run (!^ NonGit.Msg.Build) collaborator
    |> ignore

Target.create "Test" <| fun _ ->
    Collaborator.run (!^ NonGit.Msg.Test) collaborator
    |> ignore     

Target.create "publishToLocalNugetServer" <| fun _ ->
    Collaborator.run (!^ (Forker.Msg.PublishToLocalNugetServer LocalNugetServer.DefaultValue)) collaborator
    |> ignore

    
Target.create "nextRelease" <| fun _ ->
    Collaborator.run Collaborator.Msg.NextRelease collaborator
    |> ignore

Target.create "Default" ignore

Target.runOrDefault "Default"