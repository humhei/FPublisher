namespace FPublisher
open System
open Fake.Tools.Git
open FakeHelper.CommandHelper
open System.IO
open Utils
module Git =

    [<RequireQualifiedAccess>]
    type RepoState =
        | Changed
        | None  

    [<RequireQualifiedAccess>]
    module Workspace =
        let repoName workspace = 
            let _, lines, _ = Workspace.git (sprintf "config --get remote.origin.url") workspace
            lines |> List.exactlyOne |> Path.GetFileNameWithoutExtension 

        /// stageAll and then run diff            
        let diff (Workspace dir) =
            Git.diff dir

        let isRepo workspace =
            let _, lines, _ = Workspace.git (sprintf "config --get remote.origin.url") workspace
            lines.Length <> 0

        let branchName workspace = 
            let _, lines, _ = Workspace.git (sprintf "rev-parse --abbrev-ref HEAD") workspace
            lines |> List.exactlyOne    
              
        let commitNumber branchName workspace = 
            let _, lines, _ = Workspace.git (sprintf "rev-list --count %s" branchName) workspace
            lines |> List.exactlyOne |> Int32.Parse
        
        let commitHashRemote branchName workspace = 
            let _, lines, _ = Workspace.git (sprintf "rev-parse origin/%s" branchName) workspace
            lines |> List.exactlyOne

        let commitHashLocal branchName workspace = 
            let _, lines, _ = Workspace.git (sprintf "rev-parse %s" branchName) workspace
            lines |> List.exactlyOne
            
        let unPushed workspace =
            let _, lines, _ = Workspace.git "cherry -v" workspace
            lines

        let internal repoStateWith diffFilter workspace =
            let diffLines = diff workspace |> diffFilter
            let unPushedLines = unPushed workspace |> diffFilter

            match diffLines, unPushedLines with 
            | [], [] -> RepoState.None
            | _ -> RepoState.Changed

        let repoState workspace = repoStateWith id workspace

        let gitPush commitMsg workspace = async {
            match repoState workspace with 
            | RepoState.Changed ->
                Staging.stageAll workspace.WorkingDir
                Commit.exec workspace.WorkingDir commitMsg
                Branches.push workspace.WorkingDir
            | RepoState.None -> ()           
        }

