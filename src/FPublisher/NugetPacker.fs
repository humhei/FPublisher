namespace FPublisher.Nuget
open FPublisher
open Fake.Core
open Fake.IO
open System.IO
open Fake.IO.FileSystemOperators
open FakeHelper.CommandHelper
open Utils
open Fake.IO.Globbing.Operators
open Fake.DotNet.NuGet
open FSharp.Data
open Newtonsoft.Json
open Octokit
open FakeHelper
open FakeHelper.Build
open FPublisher.Solution
open FPublisher.GitHub


module NugetPacker =

    [<RequireQualifiedAccess>]
    module Workspace =
        let workaroundPaketNuSpecBug (workspace: Workspace) =
            let root = workspace.WorkingDir
            !! (root </> "./*/obj/**/*.nuspec")
            |> File.deleteAll


    let [<Literal>] officalNugetV3SearchQueryServiceUrl = "https://api-v2v3search-0.nuget.org/query"


    [<RequireQualifiedAccess>]
    type NugetAuthors =
        | GithubLoginName
        | ManualInput of string list


    [<RequireQualifiedAccess>]
    module NugetAuthors =
        let authorsTextList (repository: Repository) (nugetAuthors: NugetAuthors) =
            match nugetAuthors with
            | NugetAuthors.GithubLoginName -> [repository.Owner.Login]
            | NugetAuthors.ManualInput authors -> authors

        let authorsText (repository: Repository) (nugetAuthors: NugetAuthors) =
            match nugetAuthors with
            | NugetAuthors.GithubLoginName -> repository.Owner.Login
            | NugetAuthors.ManualInput authors -> String.separated ";" authors





    [<RequireQualifiedAccess>]
    module PackPackageResult =
        let testSourceLink = ()


    [<RequireQualifiedAccess>]
    type SourceLinkCreate =
        | LibraryAndCli
        | Library
        | None

    [<RequireQualifiedAccess>]
    module SourceLinkCreate =
        let addSourceLinkPackages (solution: Solution)  = function
            | SourceLinkCreate.LibraryAndCli ->
                solution.LibraryProjects @ solution.CliProjects
                |> List.iter (Project.addPackage "Microsoft.SourceLink.GitHub" "1.0.0-beta2-18618-05")

            | SourceLinkCreate.Library ->
                solution.LibraryProjects
                |> List.iter (Project.addPackage "Microsoft.SourceLink.GitHub" "1.0.0-beta2-18618-05")

            | SourceLinkCreate.None ->
                logger.Info "source link is disable, skip add sourcelink package"

    type NugetPacker =
        { Authors: NugetAuthors
          GenerateDocumentationFile: bool
          SourceLinkCreate: SourceLinkCreate
          PackageIconUrl: string option }
    with
        static member DefaultValue =
            { Authors = NugetAuthors.GithubLoginName
              GenerateDocumentationFile = false
              SourceLinkCreate = SourceLinkCreate.None
              PackageIconUrl = None }



    [<RequireQualifiedAccess>]
    module NugetPacker =
        let addSourceLinkPackages (solution: Solution) nugetPackager =
            SourceLinkCreate.addSourceLinkPackages solution nugetPackager.SourceLinkCreate

        let testSourceLink (packResult: PackResult) nugetPacker =
            let testSourceLinkForPackPackageResult (packPackageResult: PackPackageResult) =
                let sourceLink = dotnetGlobalTool "sourceLink"
                match Project.buildOutputInPackages packPackageResult.OriginProject.ProjPath with 
                | [] ->
                    exec sourceLink ["test" ;packPackageResult.Path] "./"
                | _ ->
                    try 
                        exec sourceLink ["test" ;packPackageResult.Path] "./"
                    with ex -> printf "%s" ex.Message 

            match nugetPacker.SourceLinkCreate with 
            | SourceLinkCreate.LibraryAndCli ->
                packResult.LibraryPackages @ packResult.CliPackages |> List.iter testSourceLinkForPackPackageResult
            | SourceLinkCreate.Library ->
                packResult.LibraryPackages |> List.iter testSourceLinkForPackPackageResult
            | SourceLinkCreate.None ->
                logger.Info "source link is disable, skip test source link"

        let updatePackOptions (githubData: CommonGitHubData) releaseNotes (ops: FPublisher.DotNet.PackOptions) (nugetPacker: NugetPacker) =
            let repository = githubData.Repository
            let topics = githubData.Topics
            let license = githubData.License

            { ops with 
                Authors =  NugetAuthors.authorsTextList repository nugetPacker.Authors
                GenerateDocumentationFile = nugetPacker.GenerateDocumentationFile
                Description = Some repository.Description
                ReleaseNotes = Some releaseNotes
                Tags = topics.names
                PackageIconUrl = nugetPacker.PackageIconUrl
                PackageProjectUrl = Some repository.HtmlUrl
                PackageLicenseUrl = Some license.HtmlUrl
                Version = Some releaseNotes.NugetVersion
            }

        let pack (solution: Solution) (githubData: CommonGitHubData) releaseNotes setParams (nugetPacker: NugetPacker) =
            Solution.workaroundPaketNuSpecBug solution
            
            let setParams (ops: FPublisher.DotNet.PackOptions) =
                let ops: FPublisher.DotNet.PackOptions = setParams ops
                updatePackOptions githubData releaseNotes ops nugetPacker

            Solution.pack setParams solution



    open FPublisher.Nuget.Nuget


    [<RequireQualifiedAccess>]
    module NugetServer =

        let createBagetLocalWithPort port = 
            { ApiEnvironmentName = None
              Serviceable = sprintf "http://127.0.0.1:%s/v3/index.json" port
              SearchQueryService = sprintf "http://127.0.0.1:%s/v3/search" port} 


        let ping (nugetServer: NugetServer) =
            try
                let response = Http.Request(nugetServer.Serviceable,silentHttpErrors = true)
                if response.StatusCode >= 400 then None
                else Some nugetServer
            with error ->
                None

        let publish (packages: string list) (nugetServer: NugetServer) = async {
            let targetDirName = Path.GetTempPath() </> (Path.GetRandomFileName())

            Directory.ensure targetDirName

            packages |> Shell.copyFiles targetDirName
            !! (targetDirName </> "./*.nupkg")
            |> Seq.map (fun nupkg -> async {

                dotnet "nuget"
                    [
                        yield! [ "push"; nupkg; "-s"; nugetServer.Serviceable ]
                        match nugetServer.ApiEnvironmentName with
                        | Some envName ->
                            let nuget_api_key = Environment.environVarOrFail envName
                            TraceSecrets.register envName nuget_api_key
                            yield! [ "-k"; nuget_api_key ]
                        | None -> ()
                    ] targetDirName
                }
            )
            |> Async.Parallel
            |> Async.RunSynchronously
            |> ignore
        }

        let getLastVersion (solution: Solution) nugetServer =
            Solution.lastVersionFromCustomNugetServer nugetServer.SearchQueryService solution


    type OfficalNugetServer =
        { ApiEnvironmentName: string }

    with
        member x.ApiKey = Environment.environVarOrFail x.ApiEnvironmentName



    [<RequireQualifiedAccess>]
    module OfficalNugetServer =
        let asNugetServer (officalNugetServer: OfficalNugetServer) : NugetServer =
            { ApiEnvironmentName = Some officalNugetServer.ApiEnvironmentName;
              Serviceable = "https://api.nuget.org/v3/index.json"
              SearchQueryService = officalNugetV3SearchQueryServiceUrl }

        let publish (packages: string list) (officalNugetServer: OfficalNugetServer) =
            let nugetServer = asNugetServer officalNugetServer
            NugetServer.publish packages nugetServer

        let getLastVersion (solution: Solution) (officalNugetServer: OfficalNugetServer) =
            let nugetServer = asNugetServer officalNugetServer
            NugetServer.getLastVersion solution nugetServer