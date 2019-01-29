namespace FPublisher
open System
open Fake.Tools.Git
open FakeHelper.CommandHelper
open System.IO

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
            
        let repoState workspace = 
            let msgs = diff workspace
            match msgs with 
            | [] -> RepoState.None
            | _ -> RepoState.Changed

        let gitPush commitMsg workspace = async {
            match repoState workspace with 
            | RepoState.Changed ->
                Staging.stageAll workspace.WorkingDir
                Commit.exec workspace.WorkingDir commitMsg
                Branches.push workspace.WorkingDir
            | RepoState.None -> ()           
        }


