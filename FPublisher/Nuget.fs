namespace FPublisher
open Fake.Core
open Fake.IO
open System.IO
open Fake.Core.SemVerActivePattern
open Fake.DotNet
open Fake.IO.FileSystemOperators
open FakeHelper.CommandHelper
open Utils
open GitHub
open Fake.IO.Globbing.Operators
open Fake.DotNet.NuGet
open FSharp.Data
open Newtonsoft.Json
open Octokit
open FakeHelper

module Nuget =

    [<RequireQualifiedAccess>]
    module Workspace =
        let workaroundPaketNuSpecBug (workspace: Workspace) =
            let root = workspace.WorkingDir
            !! (root + "./*/obj/**/*.nuspec")
            |> File.deleteAll  
    

    let [<Literal>] officalNugetV3SearchQueryServiceUrl = "https://api-v2v3search-0.nuget.org/query"

    [<RequireQualifiedAccess>]
    module Solution =        

        let private versionFromServer getLastVersion server solution = async {
            let versions = 
                Solution.nugetPackageNames solution
                |> Seq.map (fun packageName ->
                    async {return getLastVersion server packageName}    
                )
                |> Async.Parallel
                |> Async.RunSynchronously
                |> Array.choose id
                
            if versions.Length > 0 then return Some (Array.max versions) 
            else return None       
        }

        let workaroundPaketNuSpecBug (solution: Solution) =
            solution.Projects
            |> Seq.collect (fun proj ->
                let dir = Path.getDirectory proj.ProjPath
                !! (dir + "./obj/**/*.nuspec")
            )
            |> File.deleteAll  


        let lastStableVersionFromNugetServerV2 (solution: Solution) = versionFromServer Version.getLastNuGetVersion  "https://www.nuget.org/api/v2" solution

        type NugetSearchItemResultV3 =
            { version: string }

        type private NugetSearchResultV3 =
            { data: NugetSearchItemResultV3 list}

        let private getLastNugetVersionV3 server packageName = 
            let json = Http.RequestString (server,["q",packageName;"prerelease","true"]) 
            let result = JsonConvert.DeserializeObject<NugetSearchResultV3> json
            result.data 
            |> List.tryHead
            |> Option.map (fun nugetItem ->
                SemVer.parse nugetItem.version
            )
        

        let lastVersionFromCustomNugetServer server (solution: Solution) = versionFromServer getLastNugetVersionV3 server solution
        let lastVersionFromOfficalNugetServer (solution: Solution) = versionFromServer getLastNugetVersionV3 officalNugetV3SearchQueryServiceUrl solution


        

    type NugetPackage =
        { Name: string
          Version: SemVerInfo
          Path: string }
          
    [<RequireQualifiedAccess>]          
    module NugetPackage =
        let private pattern =
            @"(?<name>[\w.]+)" +
            @"(\.(?<major>\d+))" +
            @"(\.(?<minor>\d+))" +
            @"(\.(?<patch>\d+))" +
            @"(\-(?<pre>[0-9A-Za-z\-\.]+))?" +
            @"(\.(?<build>[0-9A-Za-z\-\.]+))?$"  

        let tryCreateByPath (fullPath: string) =
            
            let fileName = Path.GetFileNameWithoutExtension fullPath

            match fileName with
            | ParseRegex pattern (packageName :: _) ->
                let versionText = fileName.Replace (packageName + ".", "")
                
                { Name = packageName
                  Version = SemVer.parse versionText 
                  Path = fullPath }
                |> Some
            | _ -> None   


    [<RequireQualifiedAccess>]
    type NugetAuthors = 
        | GithubLoginName
        | ManualInput of string list


    [<RequireQualifiedAccess>]
    module NugetAuthors = 
        let authorsText (repository: Repository) (nugetAuthors: NugetAuthors) =
            match nugetAuthors with 
            | NugetAuthors.GithubLoginName -> repository.Owner.Login
            | NugetAuthors.ManualInput authors -> String.separated ";" authors

    type NugetPacker = 
        { Authors: NugetAuthors
          GenerateDocumentationFile: bool
          SourceLinkCreate: bool
          PackageIconUrl: string option
          BuildingPackOptions: DotNet.PackOptions -> DotNet.PackOptions }
    

    [<RequireQualifiedAccess>]
    module NugetPacker =
        
        let pack (solution: Solution) (nobuild:bool) (topics: Topics) (license: RepositoryContentLicense) (repository: Repository) (packageReleaseNotes: ReleaseNotes.ReleaseNotes) (nugetPacker: NugetPacker) =
            
            let targetDirectory = 
                let tmpDir = Path.GetTempPath()
                tmpDir </> Path.GetRandomFileName()
                |> Directory.ensureReturn

            Solution.workaroundPaketNuSpecBug solution
            
            Environment.setEnvironVar "GenerateDocumentationFile" (string nugetPacker.GenerateDocumentationFile)
            Environment.setEnvironVar "Authors" (NugetAuthors.authorsText repository nugetPacker.Authors) 
            Environment.setEnvironVar "Description"(repository.Description)
            Environment.setEnvironVar "PackageReleaseNotes" (packageReleaseNotes.Notes |> String.toLines)
            Environment.setEnvironVar "SourceLinkCreate" (string nugetPacker.SourceLinkCreate)
            Environment.setEnvironVar "PackageTags" topics.AsString
            match nugetPacker.PackageIconUrl with 
            | Some icon -> Environment.setEnvironVar "PackageIconUrl" icon
            | None -> ()
            Environment.setEnvironVar "PackageProjectUrl" repository.HtmlUrl
            Environment.setEnvironVar "PackageLicenseUrl" license.HtmlUrl
            
            let buildingPackOptions (options: DotNet.PackOptions) =  
                let basicBuildingOptions (options: DotNet.PackOptions) =
                    let versionParam = "/p:Version=" + packageReleaseNotes.NugetVersion
                    { options with 
                        NoBuild = nobuild
                        Configuration = DotNet.BuildConfiguration.Debug
                        OutputPath = Some targetDirectory
                        Common = { options.Common with CustomParams = Some versionParam }}

                let buildingOptionsUser = nugetPacker.BuildingPackOptions

                options                    
                |> basicBuildingOptions
                |> buildingOptionsUser
                |> dtntSmpl      
                    
            DotNet.pack buildingPackOptions solution.Path 

            !! (targetDirectory + "./*.nupkg")
            |> List.ofSeq 

    type NugetServer =
        { ApiEnvironmentName: string option
          Serviceable: string
          SearchQueryService: string }


    [<RequireQualifiedAccess>]
    module NugetServer =
        let publish (packages: string list) (nugetServer: NugetServer) = async {
            let targetDirName = Path.GetTempPath() </> (Path.GetRandomFileName())

            Directory.ensure targetDirName

            packages |> Shell.copyFiles targetDirName

            dotnet targetDirName
                "nuget" 
                [
                    yield! [ "push"; "*.nupkg"; "-s"; nugetServer.Serviceable ] 
                    match nugetServer.ApiEnvironmentName with 
                    | Some envName -> 
                        yield! [ "-k"; Environment.environVarOrFail envName ]
                    | None -> ()
                ]
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

    [<RequireQualifiedAccess>]
    type LocalNugetServer = 
        | BaGet of serviceable: string * searchQueryService: string
    with 
        member x.AsNugetServer = 
            match x with 
            | LocalNugetServer.BaGet (serviceable, searchQueryService) -> 
                { ApiEnvironmentName = None; Serviceable = serviceable; SearchQueryService = searchQueryService}

    [<RequireQualifiedAccess>]
    module LocalNugetServer =

        let publish packages (localNugetServer: LocalNugetServer) = 
            let nugetServer = localNugetServer.AsNugetServer 
            NugetServer.publish packages nugetServer

