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
open FakeHelper.Build


module Nuget =

    [<RequireQualifiedAccess>]
    module Workspace =
        let workaroundPaketNuSpecBug (workspace: Workspace) =
            let root = workspace.WorkingDir
            !! (root </> "./*/obj/**/*.nuspec")
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
                !! (dir </> "./obj/**/*.nuspec")
            )
            |> File.deleteAll


        let lastStableVersionFromNugetServerV2 (solution: Solution) = versionFromServer Version.getLastNuGetVersion  "https://www.nuget.org/api/v2" solution

        type NugetSearchItemResultV3 =
            { version: string }

        type NugetSearchResultV3 =
            { data: NugetSearchItemResultV3 list }

        let private getLastNugetVersionV3 server packageName =
            let json = Http.RequestString (server,["q",packageName;"prerelease","true"])
            let result = JsonConvert.DeserializeObject<NugetSearchResultV3> json
            result.data
            |> List.tryHead
            |> Option.map (fun nugetItem ->
                SemVerInfo.parse nugetItem.version
            )


        let lastVersionFromCustomNugetServer server (solution: Solution) = versionFromServer getLastNugetVersionV3 server solution
        let lastVersionFromOfficalNugetServer (solution: Solution) = versionFromServer getLastNugetVersionV3 officalNugetV3SearchQueryServiceUrl solution




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

    type PackResult =
        { LibraryPackages: string list
          CliPackages: string list}

    type NugetPacker =
        { Authors: NugetAuthors
          GenerateDocumentationFile: bool
          SourceLinkCreate: bool
          PackageIconUrl: string option }
    with
        static member DefaultValue =
            { Authors = NugetAuthors.GithubLoginName
              GenerateDocumentationFile = false
              SourceLinkCreate = true
              PackageIconUrl = None }



    [<RequireQualifiedAccess>]
    module NugetPacker =
        let addSourceLinkPackages (solution: Solution) nugetPackager =
            if nugetPackager.SourceLinkCreate then 
                solution.LibraryProjects @ solution.CliProjects
                |> List.iter (Project.addPackage "Microsoft.SourceLink.GitHub" "1.0.0-beta2-18618-05")
            else 
                logger.Info "source link is disable, skip add sourcelink package"

        let testSourceLink packages nugetPacker =
            if nugetPacker.SourceLinkCreate then

                let sourceLink = dotnetGlobalTool "sourceLink"

                packages |> List.iter (fun package ->
                    exec sourceLink ["test" ;package] "./"
                )
            else 
                logger.Info "source link is disable, skip test source link"


        let pack (solution: Solution) (nobuild: bool) (noRestore: bool) (topics: Topics) (license: RepositoryContentLicense) (repository: Repository) (packageReleaseNotes: ReleaseNotes.ReleaseNotes) (nugetPacker: NugetPacker) =

            let tmpDir() =
                let tmpDir = Path.GetTempPath()
                tmpDir </> Path.GetRandomFileName()
                |> Directory.ensureReturn

            Solution.workaroundPaketNuSpecBug solution

            Environment.setEnvironVar "GenerateDocumentationFile" (string nugetPacker.GenerateDocumentationFile)
            Environment.setEnvironVar "Authors" (NugetAuthors.authorsText repository nugetPacker.Authors)
            Environment.setEnvironVar "Description"(repository.Description)
            Environment.setEnvironVar "PackageReleaseNotes" (packageReleaseNotes.Notes |> String.toLines)
            Environment.setEnvironVar "PackageTags" topics.AsString
            match nugetPacker.PackageIconUrl with
            | Some icon -> Environment.setEnvironVar "PackageIconUrl" icon
            | None -> ()
            Environment.setEnvironVar "PackageProjectUrl" repository.HtmlUrl
            Environment.setEnvironVar "PackageLicenseUrl" license.HtmlUrl

            let buildingPackOptions targetDirectory customParams (options: DotNet.PackOptions) =
                let basicBuildingOptions (options: DotNet.PackOptions) =
                    let param =
                        [ yield "/p:Version=" + packageReleaseNotes.NugetVersion
                          if noRestore then
                            yield "--no-restore"
                          yield! customParams

                        ]
                        |> Args.toWindowsCommandLine

                    { options with
                        NoBuild = nobuild
                        Configuration = DotNet.BuildConfiguration.Debug
                        OutputPath = Some targetDirectory
                        Common = { options.Common with CustomParams = Some param }}

                options
                |> basicBuildingOptions
                |> dtntSmpl


            let addtionalCustomParams =
                if nugetPacker.SourceLinkCreate then
                    [ "/p:SourceLinkCreate=true"
                      "/p:AllowedOutputExtensionsInPackageBuildOutputFolder=\".dll;.exe;.winmd;.json;.pri;.xml;.pdb\"" ]
                else []

            let packProjects projects =
                let targetDirectory = tmpDir()
                projects |> List.iter (fun proj ->
                    DotNet.pack (buildingPackOptions targetDirectory addtionalCustomParams)  proj.ProjPath
                )

                !! (targetDirectory </> "./*.nupkg")
                |> List.ofSeq

            { LibraryPackages = packProjects solution.LibraryProjects
              CliPackages = packProjects solution.CliProjects }





    type NugetServer =
        { ApiEnvironmentName: string option
          Serviceable: string
          SearchQueryService: string }
    with
        static member DefaultBaGetLocal =
            { ApiEnvironmentName = None
              Serviceable ="http://localhost:5000/v3/index.json"
              SearchQueryService = "http://localhost:5000/v3/search" }

    [<RequireQualifiedAccess>]
    module NugetServer =

        let ping (nugetServer: NugetServer) =
            try
                Http.Request(nugetServer.Serviceable,silentHttpErrors = true) |> ignore
                Some nugetServer
            with _ ->
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
                            TraceSecrets.register "nuget_api_key" nuget_api_key
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
