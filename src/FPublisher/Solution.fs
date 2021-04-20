namespace FPublisher
#nowarn "0104"
open System.Xml
open FParsec
open System.IO
open Fake.IO
open Fake.IO.FileSystemOperators
open System.Text.RegularExpressions
open System.Text
open Fake.DotNet
open Fake.Core

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
          PackageReferences: PackageReferences
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
        open FPublisher.Nuget

        let create (projPath: string) =
            { OutputType = OutputType.ofProjPath projPath
              ProjPath = projPath
              TargetFrameworks = TargetFrameworks.ofProjPath projPath
              PackageReferences = PackageReferences.OfProjPath projPath
              SDK = SDK.ofProjPath projPath }


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

        let clean (project: Project) =
            let projDir = project.GetProjDir() 
            let binDir = projDir </> "bin"
            let objDir = projDir </> "obj"

            Shell.cleanDir binDir
            Shell.cleanDir objDir

            

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

        let clean (solution: Solution) =
            for project in solution.Projects do
                Project.clean project

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

        let updatablePackages nugetServer (solution: Solution) =
            solution.Projects
            |> List.collect(Project.updatablePackages nugetServer)

        let updatePackages nugetServer (solution: Solution) =
            { solution with 
                Projects =
                    solution.Projects
                    |> List.map (Project.updatePackages nugetServer)
                }



        let getPackageNames (solution: Solution) =
            solution.Projects
            |> List.map (fun project -> project.GetName())