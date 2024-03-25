namespace FPublisher.Solution

open FPublisher.FSharpPlus


#nowarn "0104"
open System.Xml
open FParsec
open System.IO
open Fake.IO
open Fake.IO.Globbing.Operators
open Fake.IO.FileSystemOperators
open System.Text.RegularExpressions
open System.Text
open Fake.Core

[<AutoOpen>]
module _SolutionTypes =

    [<RequireQualifiedAccess>]
    type TargetFramework =
        | CoreApp of name: string * float
        | FullFramework of name: string * float
        | NetStandard of string * float
        | Net of name: string * float
        | Net_Windows of name: string * float
    
    [<RequireQualifiedAccess>]
    type BuildConfiguration0 =
        | Debug 
        | Release 
        | Custom of string
    with 
        member x.Text =
            match x with 
            | Debug -> "Debug"
            | Release -> "Release"
            | Custom v -> v

        override x.ToString() = x.Text

    [<RequireQualifiedAccess>]
    module TargetFramework =
        let internal create (framework: string) =
            let framework = framework.Trim().ToLower()
            let parser =
                (pstringCI "netcoreapp" >>. pfloat .>> eof |>> (fun version -> TargetFramework.CoreApp (framework, version)))
                <|> (pstringCI "netstandard" >>. pfloat .>> eof |>> (fun version -> TargetFramework.NetStandard (framework, version)))
                <|> (pstringCI "net" >>. pfloat .>>. manyCharsTill anyChar eof |>> (fun (number, text) ->
                    match text.ToLower() with 
                    | "" ->
                        if number < 5.0 
                        then TargetFramework.FullFramework (framework, number)
                        else TargetFramework.Net (framework, number)

                    | "-windows" -> TargetFramework.Net_Windows (framework, number)
                    | _ -> failwithf "Not implemendted, cannot parse %s to TargetFramework" framework
                ))
               

            match run parser framework with 
            | Success (result, _ , _) -> result
            | Failure (msg, _, _) -> failwithf "%s" msg

        let name = function
            | TargetFramework.CoreApp (name, _) 
            | TargetFramework.FullFramework (name, _)  
            | TargetFramework.NetStandard (name, _) 
            | TargetFramework.Net (name, _) 
            | TargetFramework.Net_Windows (name, _) -> name

    [<RequireQualifiedAccess>]
    type TargetFrameworks =
        | Multiple of TargetFramework list
        | Single of TargetFramework
    with 
        member x.AsList =
            match x with 
            | Multiple v -> v
            | Single v -> [v]

    [<RequireQualifiedAccess>]
    module TargetFrameworks =
        let ofProjPath (projectFile: string) =
            let projectFile = projectFile.Replace('\\','/')
            let doc = new XmlDocument()
            let pickNode_WithoutCondition(nodes: XmlNodeList) =
                [ for node in nodes do yield node ] 
                |> function
                    | [node] -> node
                    | nodes -> 
                        nodes
                        |> List.filter(fun m -> not(m.OuterXml.Contains "Condition"))
                        |> function
                            | [node] -> node
                            | _ -> List.head nodes

            doc.Load(projectFile)
            match doc.GetElementsByTagName "TargetFramework" with
            | frameworkNodes when frameworkNodes.Count = 0 ->
                let frameworksNodes = doc.GetElementsByTagName "TargetFrameworks"
                let frameworksNode = pickNode_WithoutCondition frameworksNodes
                frameworksNode.InnerText.Split(';')
                |> Array.map (fun text -> TargetFramework.create (text.Trim()))
                |> List.ofSeq
                |> TargetFrameworks.Multiple

            | frameWorkNodes ->
                let frameworkNode = pickNode_WithoutCondition frameWorkNodes
                frameworkNode.InnerText.Trim()
                |> TargetFramework.create
                |> TargetFrameworks.Single

    type PackageReferenceTag =
        | Include = 0
        | Update = 1

    type PackageReference =
        { Name: string 
          Version: SemVerInfo
          Tag: PackageReferenceTag }
    with 
        member x.AsNodeText =
            sprintf """<PackageReference %s="%s" Version="%s"/>""" (x.Tag.ToString()) x.Name (x.Version.AsString)

        member package.RegexSearcher =
            let regexSearcher = 
                sprintf 
                    """<\s*PackageReference\s+%s\s*=\s*"%s"\s+Version\s*=\s*"%s"\s*\/> """ 
                    (package.Tag.ToString())
                    (package.Name.Replace(".", "[.]"))
                    (package.Version.AsString.Replace(".", "[.]"))

                |> fun m -> m.Trim()
            regexSearcher


    type UpdatablePackageReference =
        { Package: PackageReference
          TargetVersion: SemVerInfo }
    with 
        member x.AsNodeText =
            { x.Package with 
                Version = x.TargetVersion}.AsNodeText

    type PackageReferences = private PackageReferences of PackageReference list
    with 
        member x.AsList =
            let (PackageReferences v) = x
            v

        static member OfProjPath(projectFile: string) =
            try
                let projectFile = projectFile.Replace('\\','/')
                let doc = new XmlDocument()
                doc.Load(projectFile)
                let nodes = [ for node in doc.GetElementsByTagName "PackageReference" -> node.OuterXml ]
        
                let packages = 
                    nodes
                    |> List.map (fun m ->
                        let parser: Parser<_ ,unit> =
                            //<PackageReference Include="System.ValueTuple" Version="4.5.0" />
                            pchar '<'
                                >>.
                                spaces
                                >>. pstringCI "PackageReference" 
                                >>. 
                                (spaces1 
                                    >>. ((pstringCI "Update" <|> pstringCI "Include") 
                                        |>> (
                                            fun m -> 
                                                match m.ToLower() with 
                                                | "update" -> PackageReferenceTag.Update
                                                | "include" -> PackageReferenceTag.Include
                                                | _ -> failwith "Invalid token" 
                                            )
                                    )
                                    .>>. 
                                    (
                                        spaces 
                                        >>. pchar '='
                                        >>. spaces
                                        >>. (pchar '"' >>. (many1CharsTill anyChar (pchar '"'))
                                    )
                                )
                                .>>. 
                                (spaces1
                                    >>. pstringCI "Version"
                                    >>. spaces 
                                    >>. pchar '='
                                    >>. spaces
                                    >>. (pchar '"' >>. (many1CharsTill anyChar (pchar '"')))
                                )
                            ) 
                            |>> 
                                (fun ((tag, name), version) -> 
                                    { Tag = tag
                                      Name = name 
                                      Version = SemVer.parse version}
                                )

                        match run parser m with 
                        | Success (v, _, _) -> v
                        | Failure (error, _ , _) -> failwith "Error"
                    )
                packages
                |> List.distinctBy(fun m -> m.Name.ToLower())
                |> PackageReferences 

            with ex ->
                printfn "Parsing %s PackingReferences failed\nerrors:\n%s"  projectFile ex.Message
                reraise()






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
        | WindowsDesktop 
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
                | "Microsoft.NET.Sdk.WindowsDesktop" -> SDK.WindowsDesktop
                | others -> SDK.Others others
            | _ -> failwithf "Cannot find Project tag in project file %s" projPath


    type ProjectKind =
        | CoreCli = 0
        | Library = 1
        | Test = 2
        | AspNetCore = 3
        | Others = 4

    type ProjectReference = ProjectReference of string 
    with 
        member x.Value =
            let (ProjectReference v) = x
            v

        member x.FileNameIC =
            x.Value
            |> Path.GetFileName
            |> StringIC

    type ProjectReferences = ProjectReferences of ProjectReference list
    with 
        member x.Value =
            let (ProjectReferences v) = x
            v

        static member OfProjPath(projectFile: string) =
            let projectFile = projectFile.Replace('\\','/')
            let doc = new XmlDocument()
            doc.Load(projectFile)
            let nodes = [ for node in doc.GetElementsByTagName "ProjectReference" -> node.OuterXml ]
        
            let projectReferences = 
                nodes
                |> List.map (fun m ->
                    let parser: Parser<_ ,unit> =
                        //<PackageReference Include="System.ValueTuple" Version="4.5.0" />
                        pchar '<'
                            >>.
                            spaces
                            >>. pstringCI "ProjectReference" 
                            >>. 
                            (spaces1 
                                >>. (pstringCI "Include") 
                            
                                >>. 
                                (
                                    spaces 
                                    >>. pchar '='
                                    >>. spaces
                                    >>. (pchar '"' >>. (many1CharsTill anyChar (pchar '"'))
                                )
                            )
                        ) 
                    
                        

                    match run parser m with 
                    | Success (v, _, _) -> v
                    | Failure (error, _ , _) -> failwith "Error"
                )



            projectReferences
            |> List.distinctBy(fun m -> m.ToLower())
            |> List.map ProjectReference
            |> ProjectReferences

    type BinOrObj =
        | Bin = 0
        | Obj = 1

    type Project =
        { ProjPath: string
          OutputType: OutputType
          TargetFrameworks: TargetFrameworks
          PackageReferences: PackageReferences
          ProjectReferences: ProjectReferences
          SDK: SDK }
    with
        member x.GetName() = Path.GetFileNameWithoutExtension x.ProjPath

        member x.Name = x.GetName()

        member x.GetProjDir() = Path.getDirectory x.ProjPath

        member x.Projdir = x.GetProjDir()

        member x.GetOutputPaths (configuration: BuildConfiguration0, binOrObj: BinOrObj) =
            let projDir = x.Projdir
            let frameworkList = 
                match x.TargetFrameworks with 
                | TargetFrameworks.Multiple frameworks -> frameworks
                | TargetFrameworks.Single framework -> [framework] 
        
            frameworkList
            |> List.map (fun framework ->
                let ext = 
                    match framework, x.OutputType with 
                    | TargetFramework.CoreApp _, OutputType.Exe -> ".dll"
                    | TargetFramework.NetStandard _, OutputType.Library -> ".dll"
                    | TargetFramework.FullFramework _, OutputType.Library -> ".dll"
                    | TargetFramework.FullFramework _, OutputType.Exe -> ".exe"
                    | TargetFramework.Net _, OutputType.Library -> ".dll"
                    | TargetFramework.Net _, OutputType.Exe -> ".exe"
                    | _ -> failwithf "target framework %A is not supported when output type is %A" framework x.OutputType
                let fileName = x.GetName() + ext

                let outputDir = sprintf "%s/%O/%s" (binOrObj.ToString().ToLower()) configuration.Text (TargetFramework.name framework)
                projDir </> outputDir </> fileName
            )
 
        member x.GetOutputPaths (configuration: BuildConfiguration0) =
            x.GetOutputPaths(configuration, BinOrObj.Bin)

        member x.GetOutputDirs configuration =
            x.GetOutputPaths configuration
            |> List.map Path.getDirectory

        member x.GetProjectKind() =
            match x.OutputType, x.SDK, x.TargetFrameworks with 
            | OutputType.Exe, SDK.Microsoft_NET_Sdk, TargetFrameworks.Single (TargetFramework.CoreApp _) ->
                if x.GetName().Contains("test", true) then ProjectKind.Test
                else ProjectKind.CoreCli

            | OutputType.Exe, SDK.Microsoft_NET_Sdk, TargetFrameworks.Single (TargetFramework.Net _) ->
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
              PackageReferences = PackageReferences.OfProjPath projPath
              SDK = SDK.ofProjPath projPath
              ProjectReferences = ProjectReferences.OfProjPath projPath
              }

        let buildOutputInPackages (projPath: string) =
            let doc = new XmlDocument()

            doc.Load(projPath)

            match doc.GetElementsByTagName "BuildOutputInPackage" with
            | nodes when nodes.Count = 0 -> []
            | nodes ->
                [ for node in nodes do
                    yield node.InnerText ]

        let existFullFramework (project: Project) = 
            project.TargetFrameworks.AsList
            |> List.exists (fun framework ->
                match framework with 
                | TargetFramework.FullFramework _ -> true
                | _ -> false
            )

        let clean (project: Project) =
            let projDir = project.GetProjDir() 
            let binDir = projDir </> "bin"
            let objDir = projDir </> "obj"

            Shell.cleanDir binDir
            Shell.cleanDir objDir


    type Solution =
        { Path: string
          Projects: Project list }

    with
        member x.WorkingDir = Path.getDirectory x.Path

        member x.CliProjects =
            x.Projects |> List.filter (fun project ->
                match project.OutputType with
                | OutputType.Exe -> 
                    not (project.Name.Contains "test" || project.Name.Contains "Test") 
                    && project.SDK <> SDK.Microsoft_NET_Sdk_Web
                    && project.SDK <> SDK.WindowsDesktop
                | OutputType.Library -> false
            )

        member x.TestProjects =
            x.Projects |> List.filter (fun project ->
                match project.OutputType with
                | OutputType.Exe -> 
                    (project.Name.Contains "test" || project.Name.Contains "Test")
                    && project.SDK <> SDK.Microsoft_NET_Sdk_Web
                    && project.SDK <> SDK.WindowsDesktop

                | OutputType.Library -> false
            )

        member x.LibraryProjects =
            x.Projects |> List.filter (fun project ->
                project.OutputType = OutputType.Library
            )

        member x.AspNetCoreProjects =
            x.Projects |> List.filter (fun project ->
                match project.OutputType with
                | OutputType.Exe -> 
                    match project.SDK with 
                    | SDK.Microsoft_NET_Sdk_Web -> true
                    | _ -> false

                | OutputType.Library -> false
            )

        member x.TargetTestDlls(buildConfiguration) =
            x.TestProjects |> List.map (fun testProj -> testProj.GetOutputPaths(buildConfiguration))

    [<RequireQualifiedAccess>]
    module Solution =   
        let checkValidSlnPath path =
            if Path.GetExtension path <> ".sln" then failwithf "%s is a valid sln path" path


    [<RequireQualifiedAccess>]
    module Workspace =
        let cleanBinAndObjForDirs (dirs: seq<string>) =
            if Seq.isEmpty dirs then ()
            else
                dirs
                |> Seq.collect (fun dir -> [dir </> "obj"; dir </> "bin"])
                |> Shell.cleanDirs
