namespace FPublisher
open Fake.IO
open System.Text
open System.Text.RegularExpressions
open System.IO
open Fake.IO.FileSystemOperators
open FakeHelper.CommandHelper
open CrackedFsproj
open Fake.Core
open FakeHelper.Build

type Solution =
    { Path: string
      Projects: CrackedFsproj list }
with
    member x.WorkingDir = Path.getDirectory x.Path

    member x.TestProjects =
        x.Projects |> List.filter (fun project ->
            match project.ProjectTarget with
            | ProjectTarget.Exe -> project.Name.Contains "test" || project.Name.Contains "Test"
            | ProjectTarget.Library -> false
        )

    member x.LibraryProjects =
        x.Projects |> List.filter (fun project ->
            project.ProjectTarget = ProjectTarget.Library
        )

[<RequireQualifiedAccess>]
module Solution =

    let nugetPackageNames (solution: Solution) =
        solution.Projects
        |> List.map (fun fsproj ->
            Path.GetFileNameWithoutExtension fsproj.ProjPath
        )

    let private pattern = "Project[\(\"\{ \}\)\w\-]+\=[ ]+\"(?<name>[\w.]+)\",[ ]+\"(?<relativePath>[\w\\\.]+)\""

    let checkValidSlnPath path =
        if Path.GetExtension path <> ".sln" then failwithf "%s is a valid sln path" path

    let read slnPath =
        checkValidSlnPath slnPath
        let workingDir = Path.getDirectory slnPath

        dotnet workingDir "restore" [slnPath]

        { Path = slnPath
          Projects =
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
                CrackedFsproj.create projPath
            )
            |> Async.Parallel
            |> Async.RunSynchronously
            |> List.ofSeq }

    let restore (solution: Solution) =
        dotnet solution.WorkingDir "restore" [solution.Path]

    let build versionOp (solution: Solution) =
        match versionOp with 
        | Some (version: SemVerInfo) -> 
            let versionText = SemVerInfo.normalize version
            dotnet solution.WorkingDir "build" [solution.Path; "-p:Version=" + versionText]
        | None -> dotnet solution.WorkingDir "build" [solution.Path]

    let test (solution: Solution) =
        solution.TestProjects
        |> List.collect (fun crackedFsproj ->
            crackedFsproj.Value
        )
        |> List.map (fun crackedProjSingleTarget ->
            let targetPath = crackedProjSingleTarget.TargetPath
            let dir = Path.getDirectory targetPath
            async {
                dotnet dir crackedProjSingleTarget.TargetPath ["--summary"]

                let dll = crackedProjSingleTarget.TargetFileName

                let testResultXml =
                    let name = Path.GetFileNameWithoutExtension dll + ".TestResults.xml"
                    crackedProjSingleTarget.TargetDir </> name
                if File.Exists testResultXml then
                    Trace.publish (ImportData.Nunit NunitDataVersion.Nunit) testResultXml
            }
        )
        |> Async.Parallel
        |> Async.RunSynchronously
        |> ignore