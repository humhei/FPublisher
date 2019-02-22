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

    [<RequireQualifiedAccess>]
    type ProjectTarget =
        | Exe
        | Library

    type CrackedFsprojSingleTarget =
        { FSharpProjectOptions: FSharpProjectOptions
          ProjRefs: string list
          Props: Map<string,string>
          ProjPath: string }

    with
        member x.ProjectTarget =
            x.FSharpProjectOptions.OtherOptions |> Array.find (fun op ->
                op.StartsWith "--target"
            )
            |> function
                | "--target:exe" -> ProjectTarget.Exe
                | "--target:library" -> ProjectTarget.Library
                | others -> failwithf "unknown target compile option %s" others


        member x.TargetPath = x.Props.["TargetPath"]

        member x.TargetDir = Path.getDirectory x.TargetPath

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
        member x.AsList =
            let (CrackedFsproj value) = x
            value

        member x.ProjectTarget = x.AsList.[0].ProjectTarget

        member x.ProjRefs = x.AsList.[0].ProjRefs

        member x.ProjPath = x.AsList.[0].ProjPath

        member x.Name = Path.GetFileNameWithoutExtension x.ProjPath

        member x.SourceFiles =
            x.AsList.[0].FSharpProjectOptions.OtherOptions
            |> Array.filter(fun op -> op.EndsWith ".fs" && not <| op.EndsWith "AssemblyInfo.fs" )
            |> Array.map Path.getFullName

    [<RequireQualifiedAccess>]
    module CrackedFsproj =

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

       ///async works may fail due to fetch proj options is not thread safe
       /// retry many times will solve it
        let private fetchUnsafeDataAsync maxRetryCount taskInterval task prediate taskResultToTaskArgMapping allTaskArgs = async {
            let rec loop retryCount accum allTaskArgs =
                printfn "try fetch unsafe thread datas at %d time" retryCount

                if retryCount > maxRetryCount then failwith "exceed max retry times"

                if Seq.isEmpty allTaskArgs then accum
                else
                    let allTaskResults =
                        allTaskArgs
                        |> Seq.mapi (fun i project -> async {
                            /// Set time delay to reduce the mistake times
                            do! Async.Sleep (taskInterval * i)
                            return! task project
                        }
                        )
                        |> Async.Parallel
                        |> Async.RunSynchronously

                    let success,unsuccess = allTaskResults |> Array.partition prediate

                    loop (retryCount + 1) success (unsuccess |> Array.map taskResultToTaskArgMapping)

            return loop 1 [||] allTaskArgs
        }

        let createFromProjects projPaths =

            let prediate (crackedFsproj: CrackedFsproj) =
                crackedFsproj.AsList
                |> List.exists (fun crackedFsprojSingleTarget ->
                    crackedFsprojSingleTarget.FSharpProjectOptions.OtherOptions.Length <> 0
                )

            fetchUnsafeDataAsync
                3
                50
                create
                prediate
                (fun crackedFsproj -> crackedFsproj.ProjPath) (Array.ofSeq projPaths)