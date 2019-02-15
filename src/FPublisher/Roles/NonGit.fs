namespace FPublisher.Roles
open FPublisher
open FPublisher.Nuget
open Fake.IO.FileSystemOperators
open FPublisher.Git
open Primitives
open Fake.IO.Globbing.Operators

[<RequireQualifiedAccess>]
module NonGit =

    [<RequireQualifiedAccess>]
    type Msg =
        | Build
        | Test

    type TargetState =
        { Build: BoxedState
          Test: BoxedState }

    [<RequireQualifiedAccess>]
    module TargetState =
        let init =
            { Build = State.Init
              Test = State.Init }

    type Role =
        { Solution: Solution
          Workspace: Workspace
          TargetState: TargetState }
    with
        interface IRole<TargetState>

    let create loggerLevel (workspace: Workspace) =
        logger <- Logger.create(loggerLevel)
        let slnPath =
            let slnName =
                match Workspace.tryRepoName workspace with
                | Some repoName -> repoName
                | None -> workspace.DefaultSlnName
            workspace.WorkingDir </> (slnName  + ".sln")

        Workspace.createSlnWith slnPath false workspace

        { Solution = Solution.read slnPath
          Workspace = workspace
          TargetState = TargetState.init }


    let run =
        Role.update (function
            | Msg.Build ->
                { PreviousMsgs = []
                  Action = MapState (fun role -> Solution.build role.Solution) }
            | Msg.Test ->
                { PreviousMsgs = [Msg.Build]
                  Action = MapState (fun role -> Solution.test role.Solution) }
        )