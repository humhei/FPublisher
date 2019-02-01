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
module Nuget =

    [<RequireQualifiedAccess>]
    module Workspace =
        let workaroundPaketNuSpecBug (workspace: Workspace) =
            let root = workspace.WorkingDir
            !! (root + "./*/obj/**/*.nuspec")
            |> File.deleteAll  

        let versionFromNugetServer (workspace: Workspace) = async {
            let versions = 
                Workspace.nugetPackageNames workspace
                |> Seq.map (fun packageName ->
                    async {return Version.getLastNuGetVersion "https://www.nuget.org/api/v2" packageName}    
                )
                |> Async.Parallel
                |> Async.RunSynchronously
                |> Array.choose id
                
            if versions.Length > 0 then return Some (Array.max versions) 
            else return None   
        }

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
        let authorsText (githubData: GitHubData) (nugetAuthors: NugetAuthors) =
            match nugetAuthors with 
            | NugetAuthors.GithubLoginName -> githubData.LoginName
            | NugetAuthors.ManualInput authors ->
                String.separated ";" authors

    type NugetPacker = 
        { Authors: NugetAuthors
          GenerateDocumentationFile: bool
          SourceLinkCreate: bool
          PackageIconUrl: string option
          BuildingPackOptions: DotNet.PackOptions -> DotNet.PackOptions }


    [<RequireQualifiedAccess>]
    module NugetPacker =

        let pack slnPath (workspace: Workspace) githubData targetDirectory (packageReleaseNotes: ReleaseNotes.ReleaseNotes) (nugetPacker: NugetPacker) =
            Workspace.workaroundPaketNuSpecBug workspace
            
            let githubRepository = githubData.Repository
            Environment.setEnvironVar "GenerateDocumentationFile" (string nugetPacker.GenerateDocumentationFile)
            Environment.setEnvironVar "Authors" (NugetAuthors.authorsText githubData nugetPacker.Authors) 
            Environment.setEnvironVar "Description"(githubRepository.Description)
            Environment.setEnvironVar "PackageReleaseNotes" (packageReleaseNotes.Notes |> String.toLines)
            Environment.setEnvironVar "SourceLinkCreate" (string nugetPacker.SourceLinkCreate)
            Environment.setEnvironVar "PackageTags" githubData.Topics.AsString
            match nugetPacker.PackageIconUrl with 
            | Some icon -> Environment.setEnvironVar "PackageIconUrl" icon
            | None -> ()
            Environment.setEnvironVar "PackageProjectUrl" githubRepository.HtmlUrl
            Environment.setEnvironVar "PackageLicenseUrl" githubData.License.HtmlUrl
            
            let buildingPackOptions (options: DotNet.PackOptions) =  
                let basicBuildingOptions (options: DotNet.PackOptions) =
                    let versionParam = "/p:Version=" + packageReleaseNotes.NugetVersion
                    { options with 
                        Configuration = DotNet.BuildConfiguration.Debug
                        OutputPath = Some targetDirectory
                        Common = { options.Common with CustomParams = Some versionParam }}

                let buildingOptionsUser = nugetPacker.BuildingPackOptions

                options                    
                |> basicBuildingOptions
                |> buildingOptionsUser
                |> dtntSmpl      
                    
            DotNet.pack buildingPackOptions slnPath                

    type NugetPublisher =
        { ApiEnvironmentName: string }

    with 
        member x.ApiKey = Environment.environVarOrFail x.ApiEnvironmentName



    [<RequireQualifiedAccess>]
    module NugetPublisher =
        let publish (packages: string list) (nugetPublisher: NugetPublisher) = async {
            let targetDirName = Path.GetTempPath() </> (Path.GetRandomFileName())

            Directory.ensure targetDirName

            packages |> Shell.copyFiles targetDirName

            dotnet targetDirName
                "nuget" 
                ["push"; "*.nupkg"; "-k"; nugetPublisher.ApiKey; "-s"; "https://api.nuget.org/v3/index.json"]    
        }