namespace FPublisher.Solution

open FPublisher
open Fake.DotNet.NuGet
open FSharp.Data
open Newtonsoft.Json
open FakeHelper
open FakeHelper.Build
open FPublisher.Nuget.Nuget
open FPublisher.Nuget
open FPublisher.FakeHelper.CommandHelper
open FPublisher.FSharpPlus

#nowarn "0104"
open System.Xml
open FParsec
open System.IO
open Fake.IO
open Fake.IO.FileSystemOperators
open System.Text.RegularExpressions
open System.Text
open Fake.DotNet
open Fake.IO.Globbing.Operators
open Fake.Core

[<AutoOpen>]
module _Solution =
    [<RequireQualifiedAccess>]
    module Project =
        open FPublisher.Nuget

        let updatablePackages (nugetServer: NugetServer) (project: Project) =    
            let updatablePackages = 
                project.PackageReferences.AsList
                |> List.filter(fun package ->
                    match package.Tag with 
                    | PackageReferenceTag.Include -> true
                    | PackageReferenceTag.Update -> false
                )
                |> List.choose(fun package ->
                    match NugetServer.getLastNugetVersionV3 package.Name true nugetServer with 
                    | Some version -> 
                        let serverVersion = SemVer.parse version
                        let localVersion = package.Version
                        if serverVersion > localVersion
                        then Some { Package = package; TargetVersion = serverVersion }
                        else None
                    | None -> None
                )

            updatablePackages

        let updatePackages (nugetServer: NugetServer) project =
            let packages = updatablePackages nugetServer project

            let projText = File.ReadAllText(project.ProjPath, Encoding.UTF8)

            let newProjText =
                (projText, packages)
                ||> List.fold(fun projText package ->
                    Regex.Replace(projText, package.Package.RegexSearcher, package.AsNodeText)
                ) 

            File.WriteAllText(project.ProjPath, newProjText, Encoding.UTF8)


            //for package in packages do
            //    let r = 

            //        DotNet.exec (fun op -> 
            //            {op with 
            //                WorkingDirectory = project.GetProjDir()
            //        }) (sprintf "add %s package" project.ProjPath) (sprintf "%s --source %s" package.Package.Name nugetServer.Serviceable)

            //    match r.OK with 
            //    | true -> ()
            //    | false -> failwithf "%A" r.Errors

            { project with 
                PackageReferences = PackageReferences.OfProjPath project.ProjPath
            }



    [<RequireQualifiedAccess>]
    type PublishNetCoreDependency =
        | None
        | Keep

    



    [<RequireQualifiedAccess>]
    module Solution =


        let read slnPath =
            let pattern = "Project[\(\"\{ \}\)\w\-]+\=[ ]+\"(?<name>[\w\-.]+)\",[ ]+\"(?<relativePath>[\w\\\.\-]+)\""
            let slnPath = Path.normalizeToUnixCompatible slnPath

            let checkValidSlnPath path =
                if Path.GetExtension path <> ".sln" then failwithf "%s is a valid sln path" path

            checkValidSlnPath slnPath

            let workingDir = Path.getDirectory slnPath

            let projects =
                let projPaths =
                    let input = File.readAsStringWithEncoding Encoding.UTF8 slnPath
                    [ for m in Regex.Matches(input,pattern) -> m ]

                    |> List.filter (fun m ->
                        let relativePath = m.Groups.[2].Value
                        let ext = Path.GetExtension relativePath
                        ext = ".csproj" || ext = ".fsproj"
                    )
                    |> List.map (fun m ->
                        let relativePath = m.Groups.[2].Value
                        let projPath = Path.getFullName (workingDir </> relativePath)
                        Path.normalizeToUnixCompatible projPath
                    )
                let existentProjPaths, nonexistentProjPaths =
                    projPaths
                    |> List.partition File.exists

                for path in nonexistentProjPaths do
                    logger.Warn "project file %s doesn't exist" path

                existentProjPaths
                |> List.map Project.create

            { Path = slnPath
              Projects = projects }

        let build setParams (solution: Solution) =
            DotNet.build setParams solution.Path 
          


        let clean (solution: Solution) =
            for project in solution.Projects do
                Project.clean project




        let publish publishNetCoreDependency setParams (solution: Solution) =
            let setParams (ops: DotNet.PublishOptions) =
                let ops = 
                    { ops with 
                        MSBuildParams = 
                            { ops.MSBuildParams with DisableInternalBinLog = true }
                    }
                setParams ops

            solution.AspNetCoreProjects
            |> List.iter (fun project ->
                DotNet.publish setParams project.ProjPath

                let ops = DotNet.PublishOptions.Create() |> setParams
                match publishNetCoreDependency with 
                | PublishNetCoreDependency.None ->
                    project.GetOutputDirs(ops.Configuration)
                    |> List.iter (fun outputDir ->
                        Directory.delete (outputDir </> "publish" </> "refs")
                    )
                | _ -> ()
            )

        let test configuration (solution: Solution) =
            
            let runExpectoTest() = 
                solution.TestProjects
                |> List.map (fun testProject -> async {
                    Project.exec configuration ["--summary"] testProject
                    testProject.GetOutputDirs(configuration) |> List.iter (fun outputPath ->
                        let testResultXml =
                            let name = testProject.Name + ".TestResults.xml"
                            let outputDir = Path.getDirectory outputPath
                            outputDir </> name

                        if File.Exists testResultXml then
                            Trace.publish (ImportData.Nunit NunitDataVersion.Nunit) testResultXml
                    )

                })
                |> Async.Parallel
                |> Async.RunSynchronously
                |> ignore

            let runOtherTest() =
                solution.TestProjects 
                |> List.map (fun testProject -> async {
                    let testResultXml = testProject.Projdir </> "TestResults" </> testProject.Name + ".TestResults.xml"
                    dotnet "test" ["--no-build"; "--configuration"; configuration.ToString(); "--logger"; sprintf "trx;LogFileName=%s" testResultXml] testProject.Projdir
                    if File.exists testResultXml then
                        Trace.publish (ImportData.Nunit NunitDataVersion.Nunit3) testResultXml

                })
                |> Async.Parallel
                |> Async.RunSynchronously
                |> ignore

            runExpectoTest()
            runOtherTest()


    [<AutoOpen>]
    module _Pack =
        type PackPackageResult =
            { Path: string 
              OriginProject: Project }

        type PackResult =
            { LibraryPackages: PackPackageResult list
              CliPackages: PackPackageResult list
              AspNotCorePackages: PackPackageResult list }
        with 
            member x.LibraryPackagePaths = x.LibraryPackages |> List.map (fun package -> package.Path)

            member x.CliPackagePaths = x.CliPackages |> List.map (fun package -> package.Path)


        [<RequireQualifiedAccess>]
        module Solution =
            let private projectPackingCache = new System.Collections.Concurrent.ConcurrentDictionary<_, _>()
            // ********************Pack**********************
            let pack setParams (solution: Solution): PackResult =
                let groupedProjects = 
                    solution.Projects
                    |> List.groupBy(fun project -> project.GetProjectKind())
                    |> List.filter(fun (projectKind, _) ->
                        match projectKind with 
                        | ProjectKind.CoreCli | ProjectKind.Library -> true
                        | _ -> false
                    )
            

                let ops: FPublisher.DotNet.PackOptions = setParams (FPublisher.DotNet.PackOptions.DefaultValue)

                let packages = 
                    groupedProjects
                    |> List.map(fun (projectKind, projects) ->
                        let packages = 
                            projects
                            |> List.map(fun project ->
                                projectPackingCache.GetOrAdd(FsFullPath project.ProjPath, valueFactory = fun _ ->   
                                    let outputPath = 
                                        match ops.OutputPath with 
                                        | Some outputPath -> 
                                            outputPath </> (project.Name)

                                        | None -> Path.GetTempPath() </> Path.GetRandomFileName() </> project.Name

                                    Directory.ensure outputPath

                                    let setParams (ops: FPublisher.DotNet.PackOptions) = 
                                        let ops = setParams ops
                                        { ops with OutputPath = Some outputPath }

                                    FPublisher.DotNet.pack setParams project.ProjPath
                                    let package =
                                        !! (outputPath </> "./*.nupkg") |> Seq.exactlyOne

                                    { PackPackageResult.Path = package
                                      OriginProject = project }
                                )
                            )

                        projectKind, packages
                    )
                    
                let filterByProjectKind projectKind (packges: list<ProjectKind * _>) =
                    packages
                    |> List.filter(fun (projectKind2, _) -> projectKind = projectKind2)
                    |> List.collect snd

                { LibraryPackages    = filterByProjectKind ProjectKind.Library packages
                  CliPackages        = filterByProjectKind ProjectKind.CoreCli packages
                  AspNotCorePackages = filterByProjectKind ProjectKind.AspNetCore packages }


            // ********************Update**********************
            let updatablePackages nugetServer (solution: Solution) =
                solution.Projects
                |> List.collect(Project.updatablePackages nugetServer)

            let updatePackages nugetServer (solution: Solution) =
                { solution with 
                    Projects =
                        solution.Projects
                        |> List.map (Project.updatePackages nugetServer)
                    }



            // ********************Search**********************

            let getPackageNames (solution: Solution) =
                solution.Projects
                |> List.map (fun project -> project.GetName())


            let private versionFromServer getLastVersion server solution = async {
                let versions =
                    getPackageNames solution
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
            let lastVersionFromOfficalNugetServer (solution: Solution) = versionFromServer getLastNugetVersionV3 Nuget.officalNugetV3SearchQueryServiceUrl solution
            let getLastVersion (solution: Solution) (nugetServer: Nuget.NugetServer) =
                lastVersionFromCustomNugetServer nugetServer.SearchQueryService solution

