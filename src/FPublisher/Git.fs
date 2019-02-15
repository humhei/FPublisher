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


        /// stageAll and then run diff
        let diff (Workspace dir) =
            Git.diff dir



        let tryGitUrl workspace =
            let _, lines, _ = Workspace.git (sprintf "config --get remote.origin.url") workspace
            if lines.Length = 1
            then Some lines.[0]
            elif lines.Length = 0
            then None
            else failwithf "repo name should only contain one line, current lines is %A" lines

        let tryRepoName workspace =
            tryGitUrl workspace |> Option.map Path.GetFileNameWithoutExtension

        let repoName workspace =
            match tryRepoName workspace with
            | Some repoName -> repoName
            | None -> failwith "is not a valid git repo"

        let gitUrl workspace =
            match tryGitUrl workspace with
            | Some gitUrl -> gitUrl
            | None -> failwith "is not a valid git repo"

        let isRepo workspace =
            match tryGitUrl workspace with
            | Some _ -> true
            | None -> false

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
