namespace FPublisher

open Fake.Tools.Git
open System.IO
open Fake.IO.FileSystemOperators
open Fake.IO
open Fake.Core


type Workspace = Workspace of string
with 
    member x.WorkingDir = 
        let (Workspace value) = x
        value

    member x.Name =  DirectoryInfo(x.WorkingDir).Name

    member x.PaketLockerFile = x.WorkingDir </> "paket.lock"

    member x.FakeCacheDir = x.WorkingDir </> ".fake"

    member x.DefaultSlnName = x.Name

    member x.DefaultSlnPath = x.WorkingDir </> (sprintf "%s.sln" x.DefaultSlnName)

    member x.ReleaseNotesFile = x.WorkingDir </> "RELEASE_NOTES.md"


[<RequireQualifiedAccess>]
module Workspace =
    let git args (workspace: Workspace) = CommandHelper.runGitCommand workspace.WorkingDir args

    let tryGetGitUrl workspace =
        let _, lines, _ = git (sprintf "config --get remote.origin.url") workspace
        //let _, lines, _ = git (sprintf "remote -v") workspace
        let lines =
            lines
            |> List.map(fun m -> 
                m
                    .Replace("(fetch)", "")
                    .Replace("(push)", "")
                    .Trim()
                    .Replace("origin\t", "")
                )
        if lines.Length = 1
        then Some lines.[0]
        elif lines.Length = 0
        then None
        else failwithf "repo name should only contain one lines, current lines is %A" lines

    let tryGetRepoName workspace =
        tryGetGitUrl workspace 
        |> Option.map (fun m -> 
            let fileName = Path.GetFileName m
            if (fileName).EndsWith ".git"
            then fileName.Substring(0, fileName.Length - 4)
            else fileName
        )

    let exec tool args (Workspace dir) =
        let result =
            args
            |> CreateProcess.fromRawCommand tool
            |> CreateProcess.withWorkingDirectory dir
            |> Proc.run

        if result.ExitCode <> 0
        then failwithf "Error while running %s with args %A" tool (List.ofSeq args)

    let paket args (workspace: Workspace) = 

        let paketPath (workspace: Workspace) = 
            workspace.WorkingDir </> ".paket/paket.exe"
            |> Path.convertWindowsToCurrentPath
                 
        let paketPath = paketPath workspace 


        match (File.exists paketPath), (File.exists "paket.dependencies"), ProcessUtils.tryFindFileOnPath "paket" with 
        | false, _ , None -> logger.Warn "paket file %s doesn't exist" paketPath
        | _, false, _ -> logger.Warn "paket.dependencies doesn't exist"
        | _, true, _ ->
            let paketPath =
                match (File.exists paketPath), ProcessUtils.tryFindFileOnPath "paket" with 
                | true, _ -> paketPath
                | false, Some paketPath -> paketPath
                | _ -> failwith "Invalid token"

            if Environment.isUnix 
            then
                match Mono.monoPath with 
                | Some mono -> 
                    exec mono [yield paketPath; yield! args] workspace
                | None -> failwith "Cannot find mono path in environment varaibles"
            else
                exec paketPath args workspace


