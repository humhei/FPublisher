namespace FPublisher.Roles
open FPublisher
open FPublisher.Nuget
open Fake.IO.FileSystemOperators
open FPublisher.Git
open Primitives
open Fake.IO.Globbing.Operators
open Fake.Core
open Fake.DotNet
open System.IO

[<RequireQualifiedAccess>]
module NonGit =

    [<RequireQualifiedAccess>]
    type Msg =
        | Build of SemVerInfo option
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
            let defaultSlnName =
                match Workspace.tryRepoName workspace with
                | Some repoName -> repoName
                | None -> workspace.DefaultSlnName


            let poiorSlnPath = 
                let priorSlnName = defaultSlnName + ".FPublisher"

                workspace.WorkingDir </> (priorSlnName  + ".sln")

            if File.Exists poiorSlnPath 
            then poiorSlnPath
            else workspace.WorkingDir </> (defaultSlnName  + ".sln")


        Workspace.createSlnWith slnPath false workspace
        { Solution = Solution.read slnPath
          Workspace = workspace
          TargetState = TargetState.init }


    let run =
        Role.update (function

            | Msg.Build semverInfoOp ->
                { PreviousMsgs = []
                  Action = MapState (fun role -> Solution.build semverInfoOp role.Solution) }

            | Msg.Test ->
                { PreviousMsgs = [ Msg.Build None ]
                  Action = MapState (fun role -> Solution.test role.Solution) }
        )