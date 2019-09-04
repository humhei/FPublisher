namespace FPublisher

open FSharp.Data
open Newtonsoft.Json
open Fake.Core

module Nuget =
    type NugetServer =
        { ApiEnvironmentName: string option
          Serviceable: string
          SearchQueryService: string }

    module NugetServer =

        type NugetSearchItemResultV3 =
            { version: string
              id: string }

        type NugetSearchResultV3 =
            { data: NugetSearchItemResultV3 list }

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

