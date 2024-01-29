namespace FPublisher.Nuget

open FSharp.Data
open Newtonsoft.Json
open FPublisher

module Nuget =

    let [<Literal>] officalNugetV3SearchQueryServiceUrl = "https://api-v2v3search-0.nuget.org/query"



    type NugetServer =
        { ApiEnvironmentName: string option
          Serviceable: string
          SearchQueryService: string }

        

    module NugetServer =

        
        type NugetSearchVersionResultV3 = 
            { version: string }

        type NugetSearchItemResultV3 =
            { version: string
              id: string }

        type NugetSearchItemResultWithVersionsV3 =
            { version: string
              id: string
              versions: NugetSearchVersionResultV3 list}


        type NugetSearchResultV3 =
            { data: NugetSearchItemResultV3 list
               }

        type NugetSearchResultWithVersionsV3 =
            { data: NugetSearchItemResultWithVersionsV3 list
               }


        let getAllNugetVersionsV3 packageName (includePrerelease: bool) (nugetServer: NugetServer) =

            let json = 
                Http.RequestString 
                    (nugetServer.SearchQueryService, ["q",packageName;"prerelease", includePrerelease.ToString()])

            let result = JsonConvert.DeserializeObject<NugetSearchResultWithVersionsV3> json

            result.data
            |> Seq.tryFind (fun item -> 
                item.id = packageName
            )
            |> Option.map (fun item -> item.versions |> List.map(fun m -> m.version))
            |> Option.toList
            |> List.concat

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


