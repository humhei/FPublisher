namespace FPublisher

open FSharp.Data
open Newtonsoft.Json
open Fake.Core
open Fake.DotNet
open Fake.DotNet.NuGet.NuGet
open System.IO

module Nuget =

    type _NugetServer =
        { 
          /// environment to create nuget api key
          ApiEnvironmentName: string option
          Serviceable: string
          SearchQueryService: string 
        }

    type NugetServer = private NugetServer of _NugetServer
    with 
        member x.Value =
            let (NugetServer value) = x
            value

        member x.ApiEnvironmentName = x.Value.ApiEnvironmentName

        member x.SearchQueryService = x.Value.SearchQueryService

        member x.Serviceable = x.Value.Serviceable

        member x.ApiKey = x.ApiEnvironmentName |> Option.map Environment.environVar 

        static member Create(nugetServer: _NugetServer) =
            match nugetServer.ApiEnvironmentName with 
            | Some envName ->
                Environment.environVar envName |> ignore
                TraceSecrets.register "<ApiKey>" envName

            | None -> ()

            NugetServer nugetServer

    [<RequireQualifiedAccess>]
    module NugetServer =

        type NugetSearchItemResultV3 =
            { version: string
              id: string }

        type NugetSearchResultV3 =
            { data: NugetSearchItemResultV3 list }

        let push setParams nupkg (nugetServer: NugetServer) =
            DotNet.nugetPush (fun ops -> 
                let ops = setParams ops
                {ops with 
                    PushParams = 
                        { ops.PushParams with     
                            ApiKey = nugetServer.ApiKey
                            Source = Some nugetServer.Serviceable
                        }
                }
            ) nupkg

        let getLastNugetVersionV3 packageName (includePrerelease: bool) (nugetServer: NugetServer) =

            let json = 
                Http.RequestString 
                    (nugetServer.SearchQueryService, ["q",packageName;"prerelease", includePrerelease.ToString()])

            let result = JsonConvert.DeserializeObject<NugetSearchResultV3> json

            result.data
            |> Seq.tryFind (fun item -> 
                item.id = packageName
            )
            |> Option.map (fun item -> item.version)


    type _GithubPackagesServer =
        { Name: string 
          Token: string  
          UserName: string }
    with 
        member x.Source =
            sprintf "https://nuget.pkg.github.com/%s/index.json" x.UserName


    type GithubPackagesServer(githubPackagesServer: _GithubPackagesServer) =
        let workspace = Workspace (Directory.GetCurrentDirectory())
        do
            Workspace.nuget 
                [ "Add"
                  "-Name"; githubPackagesServer.Name 
                  "-Source"; githubPackagesServer.Source 
                  "-UserName"; githubPackagesServer.UserName 
                  "-Password"; githubPackagesServer.Token ]

                workspace

        member x.Value = githubPackagesServer



    [<RequireQualifiedAccess>]
    module GithubPackagesServer =
        let push setParams nupkg (githubPackagesServer: GithubPackagesServer) =
            DotNet.nugetPush (fun ops -> 
                let ops = setParams ops
                {ops with 
                    PushParams = 
                        { ops.PushParams with     
                            Source = Some githubPackagesServer.Value.Source
                        }
                }
            ) nupkg
  


    [<RequireQualifiedAccess>]
    type PackagesServer =
        | Nuget of NugetServer
        | Github of GithubPackagesServer

    [<RequireQualifiedAccess>]
    module PackagesServer =
        let push setParams nupkg  = function
            | PackagesServer.Nuget nugetServer -> NugetServer.push setParams nupkg nugetServer
            | PackagesServer.Github githubServer -> GithubPackagesServer.push setParams nupkg githubServer