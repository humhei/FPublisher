namespace FPublisher
open Fake.IO
open System.Text
open System.Text.RegularExpressions
open System.IO
open Fake.IO.FileSystemOperators
open FakeHelper.CommandHelper
open CrackedFsproj


type Solution =
    { Path: string 
      Projects: CrackedFsproj list }
with 
    member x.WorkingDir = Path.getDirectory x.Path

    member x.TestProjects = 
        x.Projects |> List.filter (fun project ->
            project.Name.EndsWith "Tests"
        )

    member x.LibraryProjects =
        x.Projects |> List.except x.TestProjects    

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

    let build (solution: Solution) =
        dotnetWith solution.WorkingDir "build" [solution.Path]     
    
    let buildFail (solution: Solution) =
        dotnetFail solution.WorkingDir "build" [solution.Path]

    let testFail (solution: Solution) =
        solution.TestProjects
        |> List.collect (fun crackedFsproj ->
            crackedFsproj.Value
        )
        |> List.map (fun crackedProjSingleTarget ->
            let targetPath = crackedProjSingleTarget.TargetPath 
            let dir = Path.getDirectory targetPath
            async {return dotnetFail dir crackedProjSingleTarget.TargetPath [] }
        )
        |> Async.Parallel
        |> Async.RunSynchronously
        |> ignore