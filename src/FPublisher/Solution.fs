namespace FPublisher
open System.Xml
open FParsec
open System.IO
open Fake.IO
open Fake.IO.FileSystemOperators
open System.Text.RegularExpressions
open System.Text
open Fake.DotNet
open Fake.DotNet
module Solution =

    [<RequireQualifiedAccess>]
    type TargetFramework =
        | CoreApp of float
        | FullFramework of float
        | NetStandard of float
        | Unified of float

    [<RequireQualifiedAccess>]
    module TargetFramework =
        let internal create (framework: string) =
            let parser =
                (pstringCI "netcoreapp" >>. pfloat .>> eof |>> TargetFramework.CoreApp)
                <|> (pstringCI "netstandard" >>. pfloat .>> eof |>> TargetFramework.NetStandard)
                <|> (pstringCI "net" >>. pfloat .>> eof |>> (fun number ->
                    if number < 5.0 
                    then TargetFramework.FullFramework number
                    else TargetFramework.Unified number
                ))

            match run parser framework with 
            | Success (result, _ , _) -> result
            | Failure (msg, _, _) -> failwithf "%s" msg

        let name = function
            | TargetFramework.CoreApp number -> sprintf "netcoreapp%g" number
            | TargetFramework.FullFramework number  ->  sprintf "net%g" number
            | TargetFramework.NetStandard number -> sprintf "netstandard%g" number
            | TargetFramework.Unified number -> sprintf "net%g" number


    [<RequireQualifiedAccess>]
    type TargetFrameworks =
        | Multiple of TargetFramework list
        | Single of TargetFramework

    [<RequireQualifiedAccess>]
    module TargetFrameworks =
        let ofProjPath (projectFile: string) =
            let projectFile = projectFile.Replace('\\','/')
            let doc = new XmlDocument()
            doc.Load(projectFile)
            match doc.GetElementsByTagName "TargetFramework" with
            | frameworkNodes when frameworkNodes.Count = 0 ->
                let frameworksNodes = doc.GetElementsByTagName "TargetFrameworks"
                let frameworksNode = [ for node in frameworksNodes do yield node ] |> List.exactlyOne
                frameworksNode.InnerText.Split(';')
                |> Array.map (fun text -> TargetFramework.create (text.Trim()))
                |> List.ofSeq
                |> TargetFrameworks.Multiple

            | frameWorkNodes ->
                let frameworkNode = [ for node in frameWorkNodes do yield node ] |> List.exactlyOne
                frameworkNode.InnerText.Trim()
                |> TargetFramework.create
                |> TargetFrameworks.Single

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


    [<RequireQualifiedAccess>]
    type SDK =
        | Microsoft_NET_Sdk
        | Microsoft_NET_Sdk_Web
        | Others of string

    [<RequireQualifiedAccess>]
    module SDK =
        let ofProjPath (projPath: string) =
            let doc = new XmlDocument()
            doc.Load(projPath)

            match doc.GetElementsByTagName "Project" with
            | nodes when nodes.Count = 1 -> 
                let node = nodes.[0]
                let sdkAttr = node.Attributes.GetNamedItem ("Sdk")
                match sdkAttr.Value with 
                | "Microsoft.NET.Sdk" -> SDK.Microsoft_NET_Sdk
                | "Microsoft.NET.Sdk.Web" -> SDK.Microsoft_NET_Sdk_Web
                | others -> SDK.Others others
            | _ -> failwithf "Cannot find Project tag in project file %s" projPath


    type ProjectKind =
        | CoreCli = 0
        | Library = 1
        | Test = 2
        | AspNetCore = 3
        | Others = 4

    type Project =
        { ProjPath: string
          OutputType: OutputType
          TargetFrameworks: TargetFrameworks
          SDK: SDK }
    with
        member x.GetName() = Path.GetFileNameWithoutExtension x.ProjPath

        member x.GetProjDir() = Path.getDirectory x.ProjPath

        member x.GetOutputPaths (configuration: DotNet.BuildConfiguration) =
            let frameworkList = 
                match x.TargetFrameworks with 
                | TargetFrameworks.Multiple frameworks -> frameworks
                | TargetFrameworks.Single framework -> [framework] 
            
            frameworkList
            |> List.map (fun framework ->
                let ext = 
                    match framework, x.OutputType with 
                    | TargetFramework.CoreApp number, OutputType.Exe -> ".dll"
                    | TargetFramework.NetStandard _, OutputType.Library -> ".dll"
                    | TargetFramework.FullFramework _, OutputType.Library -> ".dll"
                    | TargetFramework.FullFramework _, OutputType.Exe -> ".exe"
                    | _ -> failwithf "target framework %A is not supported when output type is %A" framework x.OutputType
                let fileName = x.GetName() + ext

                let outputDir = sprintf "bin/%O/%s" configuration (TargetFramework.name framework)
                outputDir </> fileName
            )
 
        member x.GetOutputDirs configuration =
            x.GetOutputPaths configuration
            |> List.map Path.getDirectory

        member x.GetProjectKind() =
            match x.OutputType, x.SDK, x.TargetFrameworks with 
            | OutputType.Exe, SDK.Microsoft_NET_Sdk, TargetFrameworks.Single (TargetFramework.CoreApp _) ->
                if x.GetName().Contains("test", true) then ProjectKind.Test
                else ProjectKind.CoreCli

            | OutputType.Exe, SDK.Microsoft_NET_Sdk_Web, _ -> ProjectKind.AspNetCore

            | OutputType.Library, SDK.Microsoft_NET_Sdk, _ -> ProjectKind.Library
            | _ -> ProjectKind.Others

    [<RequireQualifiedAccess>]
    module Project =
        let create (projPath: string) =
            { OutputType = OutputType.ofProjPath projPath
              ProjPath = projPath
              TargetFrameworks = TargetFrameworks.ofProjPath projPath
              SDK = SDK.ofProjPath projPath }


    type Solution =
        { Path: string
          Projects: Project list }

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

        let pack setParams (solution: Solution) =

            let groupedProjects = 
                solution.Projects
                |> List.groupBy(fun project -> project.GetProjectKind())
                |> List.filter(fun (projectKind, _) ->
                    match projectKind with 
                    | ProjectKind.CoreCli | ProjectKind.Library | ProjectKind.AspNetCore -> true
                    | _ -> false
                )

            for (projectKind, projects) in groupedProjects do 
                for project in projects do
                    FPublisher.DotNet.pack setParams project.ProjPath

            groupedProjects

        let getPackageNames (solution: Solution) =
            solution.Projects
            |> List.map (fun project -> project.GetName())