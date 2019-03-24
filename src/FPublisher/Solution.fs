namespace FPublisher
open Fake.IO
open System.Text
open System.Text.RegularExpressions
open System.IO
open Fake.IO.FileSystemOperators
open FakeHelper.CommandHelper
open Fake.Core
open FakeHelper.Build
open System.Xml
open FakeHelper

[<RequireQualifiedAccess>]
type Framework =
    | MultipleTarget of string list
    | SingleTarget of string

[<RequireQualifiedAccess>]
module Framework =
    let (|CoreApp|FullFramework|NetStandard|) (framework: string) =
        if framework.StartsWith "netcoreapp" 
        then CoreApp 
        elif framework.StartsWith "netstandard"
        then NetStandard
        else FullFramework

    let asList = function
        | Framework.MultipleTarget targets -> targets
        | Framework.SingleTarget target -> [target]

    let ofProjPath (projectFile: string) =
        let projectFile = projectFile.Replace('\\','/')
        let doc = new XmlDocument()
        doc.Load(projectFile)
        match doc.GetElementsByTagName "TargetFramework" with
        | frameworkNodes when frameworkNodes.Count = 0 ->
            let frameworksNodes = doc.GetElementsByTagName "TargetFrameworks"
            let frameworksNode = [ for node in frameworksNodes do yield node ] |> List.exactlyOne
            frameworksNode.InnerText.Split(';')
            |> Array.map (fun text -> text.Trim())
            |> List.ofSeq
            |> Framework.MultipleTarget

        | frameWorkNodes ->
            let frameworkNode = [ for node in frameWorkNodes do yield node ] |> List.exactlyOne
            Framework.SingleTarget frameworkNode.InnerText

[<RequireQualifiedAccess>]
type OutputType =
    | Exe
    | Library

[<RequireQualifiedAccess>]
module OutputType =
    let ofProjPath (projPath: string) =
        let doc = new XmlDocument()

        doc.Load(projPath)

        match doc.GetElementsByTagName "OutputType" with
        | nodes when nodes.Count = 0 -> OutputType.Library
        | nodes -> 
            let nodes = 
                [ for node in nodes -> node ]
            nodes |> List.tryFind (fun node ->
                node.InnerText = "Exe"
            )
            |> function 
                | Some _ -> OutputType.Exe
                | None -> OutputType.Library
            

    let outputExt framework = function
        | OutputType.Exe -> 
            match framework with 
            | Framework.FullFramework ->
                ".exe"
            | _ -> ".dll"
        | OutputType.Library -> ".dll"

type Project =
    { ProjPath: string
      OutputType: OutputType
      TargetFramework: Framework }
with
    member x.Name = Path.GetFileNameWithoutExtension x.ProjPath

    member x.Projdir = Path.getDirectory x.ProjPath

    member x.OutputPaths =
        Framework.asList x.TargetFramework
        |> List.map (fun framework ->
            x.Projdir </> "bin/Debug" </> framework </> x.Name + OutputType.outputExt framework x.OutputType
            |> Path.nomarlizeToUnixCompitiable
        )




[<RequireQualifiedAccess>]
module Project =
    let create projPath =
        { OutputType = OutputType.ofProjPath projPath
          ProjPath = projPath
          TargetFramework = Framework.ofProjPath projPath }


    let buildOutputInPackages (projPath: string) =
        let doc = new XmlDocument()

        doc.Load(projPath)

        match doc.GetElementsByTagName "BuildOutputInPackage" with
        | nodes when nodes.Count = 0 -> []
        | nodes ->
            [ for node in nodes do
                yield node.InnerText ]

    let existFullFramework (project: Project) = 
        project.TargetFramework
        |> Framework.asList
        |> List.exists (fun framework ->
            match framework with 
            | Framework.FullFramework -> true
            | _ -> false
        )

    let exec (args: seq<string>) (project: Project) =
        project.TargetFramework
        |> Framework.asList
        |> List.iter (fun framework ->
            let outputDll =
                project.Projdir </> "bin/Debug" </> framework </> project.Name + ".dll"
                |> Path.nomarlizeToUnixCompitiable

            let outputExe = outputDll |> Path.changeExtension ".exe"

            let outputDir = Path.getDirectory outputDll

            match framework, Environment.isUnix with
            | Framework.CoreApp, _ ->
                dotnet outputDll args outputDir

            | Framework.NetStandard _,_ -> failwith "cannot exec class library"

            | Framework.FullFramework, false ->
                exec outputDll args outputDir
            | Framework.FullFramework, true ->
                match Mono.monoPath with
                | Some mono ->
                    exec mono ([outputDll; outputExe] @ List.ofSeq args) outputDir
                | None ->
                    failwith "cannot find mono"
        )

    let addPackage package version (project: Project) = 
        dotnet "add" [project.ProjPath; "package"; package; "-v"; version] (project.Projdir)


type Solution =
    { Path: string
      Projects: Project list }
with
    member x.WorkingDir = Path.getDirectory x.Path

    member x.CliProjects =
        x.Projects |> List.filter (fun project ->
            match project.OutputType with
            | OutputType.Exe -> not (project.Name.Contains "test" || project.Name.Contains "Test")
            | OutputType.Library -> false
        )

    member x.TestProjects =
        x.Projects |> List.filter (fun project ->
            match project.OutputType with
            | OutputType.Exe -> project.Name.Contains "test" || project.Name.Contains "Test"
            | OutputType.Library -> false
        )

    member x.LibraryProjects =
        x.Projects |> List.filter (fun project ->
            project.OutputType = OutputType.Library
        )

    member x.TargetTestDlls =
        x.TestProjects |> List.map (fun testProj -> testProj.OutputPaths)

[<RequireQualifiedAccess>]
module Solution =

    let nugetPackageNames (solution: Solution) =
        solution.Projects
        |> List.map (fun fsproj ->
            Path.GetFileNameWithoutExtension fsproj.ProjPath
        )

    let private pattern = "Project[\(\"\{ \}\)\w\-]+\=[ ]+\"(?<name>[\w\-.]+)\",[ ]+\"(?<relativePath>[\w\\\.\-]+)\""

    let checkValidSlnPath path =
        if Path.GetExtension path <> ".sln" then failwithf "%s is a valid sln path" path


    let read slnPath =

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
                    Path.nomarlizeToUnixCompitiable projPath
                )

            projPaths
            |> List.filter File.exists
            |> List.map Project.create

        { Path = slnPath
          Projects = projects }

        

    let restore (solution: Solution) =
        dotnet "restore" [solution.Path] solution.WorkingDir

    let build versionOp (solution: Solution) =
        match versionOp with
        | Some (version: SemVerInfo) ->
            let versionText = SemVerInfo.normalize version
            dotnet "build" [solution.Path; "-p:Version=" + versionText] solution.WorkingDir
        | None -> dotnet "build" [solution.Path] solution.WorkingDir

    let test (solution: Solution) =
        let runExpectoTest() = 
            solution.TestProjects
            |> List.map (fun testProject -> async {
                Project.exec ["--summary"] testProject
                testProject.OutputPaths |> List.iter (fun outputPath ->
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
                dotnet "test" ["--no-build"; "--logger"; sprintf "trx;LogFileName=%s" testResultXml] testProject.Projdir
                if File.exists testResultXml then
                    Trace.publish (ImportData.Nunit NunitDataVersion.Nunit3) testResultXml

            })
            |> Async.Parallel
            |> Async.RunSynchronously
            |> ignore

        runExpectoTest()
        runOtherTest()