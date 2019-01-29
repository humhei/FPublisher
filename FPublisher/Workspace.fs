namespace FPublisher
open Fake.IO
open Fake.IO.FileSystemOperators
open System.IO 
open FakeHelper
open Fake.Tools.Git.CommandHelper
open Fake.IO.Globbing.Operators
                               
type Workspace = Workspace of string
with 
    member x.WorkingDir = 
        let (Workspace value) = x
        value

    member x.Name = Path.GetFileName (x.WorkingDir.TrimEnd '\\')

    member x.PaketLockerFile = x.WorkingDir </> "paket.lock"

    member x.FakeCacheDir = x.WorkingDir </> ".fake"

    member x.SlnName = x.Name 

    member x.SlnPath = x.WorkingDir </> (sprintf "%s.sln" x.SlnName)

[<RequireQualifiedAccess>]
module Workspace =

    let cleanBinAndObjForDirs (dirs: seq<string>) =
        if Seq.isEmpty dirs then ()
        else 
            dirs 
            |> Seq.collect (fun dir -> [dir </> "obj"; dir </> "bin"])
            |> Shell.cleanDirs

    let exec tool args (workspace: Workspace) = CommandHelper.exec tool workspace.WorkingDir args
    
    let fake args (workspace: Workspace) = exec (CommandHelper.platformTool "fake") args workspace
    
    let paket args (workspace: Workspace) = exec (workspace.WorkingDir </> ".paket/paket.exe") args workspace
    
    let dotnet command args (workspace: Workspace) = CommandHelper.dotnet workspace.WorkingDir command args
    
    let git args (workspace: Workspace) = runGitCommand workspace.WorkingDir args
    
    /// include paket-files
    let allfsprojses (workspace: Workspace) =
        !! (workspace.WorkingDir + "/**/*.fsproj")
    
    /// exclude paket-files
    let fsprojses (workspace: Workspace) =
        allfsprojses workspace
        -- (workspace.WorkingDir + "/paket-files/**/*.fsproj")

    let nugetPackageNames (workspace: Workspace) = 
        fsprojses workspace
        |> Seq.map (Path.GetFileNameWithoutExtension)
        |> List.ofSeq

    let cleanBinAndObj (workspace: Workspace) =
        allfsprojses workspace
        |> cleanBinAndObjForDirs    

    let createSlnCommon isForce (workspace: Workspace) =
        let slnPath = workspace.SlnPath
        if isForce then File.delete slnPath         
        if not <| File.exists slnPath then
            dotnet "new sln" [] workspace
            fsprojses workspace
            |> Seq.iter (fun proj ->
                dotnet (sprintf "sln %s add" slnPath) [proj] workspace   
            )

    /// create sln if sln is not exists                
    let createSln (workspace: Workspace) = createSlnCommon false workspace 

    let createSlnForce (workspace: Workspace) = createSlnCommon true workspace   

