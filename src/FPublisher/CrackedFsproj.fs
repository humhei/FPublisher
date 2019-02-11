namespace FPublisher
open System.Xml
open System.IO
open Fake.IO
open Fake.IO.FileSystemOperators
open Microsoft.FSharp.Compiler.SourceCodeServices

module CrackedFsproj =
    [<RequireQualifiedAccess>]    
    module FSharpProjectOptions =
        let mapOtherOptions mapping (fsharpProjectOptions: FSharpProjectOptions) =
            { fsharpProjectOptions with 
                OtherOptions = fsharpProjectOptions.OtherOptions |> Array.map mapping }


    type CrackedFsprojSingleTarget =
        { FSharpProjectOptions: FSharpProjectOptions 
          ProjRefs: string list
          Props: Map<string,string>
          ProjPath: string }

    with 
        member x.TargetPath = x.Props.["TargetPath"]
        member x.TargetFramework = x.Props.["TargetFramework"]
        member x.TargetPdbPath = Path.changeExtension ".pdb" x.TargetPath
        member x.TargetFileName = Path.GetFileName(x.TargetPath)
        member x.TargetPdbName = Path.changeExtension ".pdb" x.TargetFileName
        member x.ObjTargetFile = 
            let projDir = Path.getDirectory x.ProjPath
            let relative = Path.toRelativeFrom projDir x.TargetPath
            let objRelative = 
                if relative.StartsWith ".\\bin" then  "obj" + relative.Substring 5
                else failwithf "is not a valid bin relativePath %s" relative
            projDir </> objRelative

        member x.ObjTargetPdb = Path.changeExtension ".pdb" x.ObjTargetFile

    [<RequireQualifiedAccess>]
    module CrackedFsprojSingleTarget =

        let mapProjOptions mapping (crackedFsprojSingleTarget: CrackedFsprojSingleTarget) =
            { crackedFsprojSingleTarget with FSharpProjectOptions = mapping crackedFsprojSingleTarget.FSharpProjectOptions }

                    
    type CrackedFsproj = private CrackedFsproj of CrackedFsprojSingleTarget list

    with    
    
        member x.Value = 
            let (CrackedFsproj value) = x
            value


        member x.ProjRefs = x.Value.[0].ProjRefs

        member x.ProjPath = x.Value.[0].ProjPath

        member x.Name = Path.GetFileNameWithoutExtension x.ProjPath    

        member x.SourceFiles = 
            x.Value.[0].FSharpProjectOptions.OtherOptions
            |> Array.filter(fun op -> op.EndsWith ".fs" && not <| op.EndsWith "AssemblyInfo.fs" )


    [<RequireQualifiedAccess>]
    module CrackedFsproj =
        [<RequireQualifiedAccess>]
        type private FrameWork = 
            | MultipleTarget of string []
            | SingleTarget of string

        let private getFrameWork (projectFile: string) =

            let doc = new XmlDocument()
            doc.Load(projectFile)
            match doc.GetElementsByTagName "TargetFramework" with 
            | frameWorkNodes when frameWorkNodes.Count = 0 -> 
                let frameWorksNodes = doc.GetElementsByTagName "TargetFrameworks" 
                let frameWorksNode = [ for node in frameWorksNodes do yield node ] |> List.exactlyOne
                frameWorksNode.InnerText.Split(';')
                |> Array.map (fun text -> text.Trim())
                |> FrameWork.MultipleTarget

            | frameWorkNodes ->
                let frameWorkNode = [ for node in frameWorkNodes do yield node ] |> List.exactlyOne
                FrameWork.SingleTarget frameWorkNode.InnerText

        let create projectFile = async {
            match! ProjectCoreCracker.getProjectOptionsFromProjectFile projectFile with 
            | [|projOptions,projRefs,props|] ->
                return 
                    { FSharpProjectOptions = projOptions
                      Props = props; ProjPath = projectFile
                      ProjRefs = projRefs }
                    |> List.singleton
                    |> CrackedFsproj             

            | [||] -> return failwithf "no frameworks is found in project file %s" projectFile
            | results -> 
                return 
                    results 
                    |> Array.map (fun (projOptions, projRefs ,props) -> { FSharpProjectOptions = projOptions; Props = props; ProjPath = projectFile; ProjRefs = projRefs }) 
                    |> List.ofSeq
                    |> CrackedFsproj
        }