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
open Fake.IO
open FPublisher.FakeHelper.CommandHelper

[<RequireQualifiedAccess>]
module NonGit =

    [<RequireQualifiedAccess>]
    type Msg =
        | InstallPaketPackages
        | Build of SemVerInfo option
        | AddSourceLinkPackages of SourceLinkCreate
        | Test
        | Zip of Project list

    type TargetState =
        { InstallPaketPackages: BoxedState
          Build: BoxedState
          AddSourceLinkPackages: BoxedState
          Test: BoxedState
          Zip: BoxedState }

    [<RequireQualifiedAccess>]
    module TargetState =
        let init =
            { InstallPaketPackages = State.Init
              AddSourceLinkPackages = State.Init
              Build = State.Init
              Test = State.Init
              Zip = State.Init }

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
            | Msg.InstallPaketPackages ->
                { PreviousMsgs = [] 
                  Action = MapState (fun role -> Workspace.paket ["install"] role.Workspace; none)}
            
            | Msg.AddSourceLinkPackages sourceLinkCreate ->
                { PreviousMsgs = [] 
                  Action = MapState (fun role ->
                    let solution = role.Solution
                    SourceLinkCreate.addSourceLinkPackages solution sourceLinkCreate
                    none
                  )}

            | Msg.Build semverInfoOp ->
                { PreviousMsgs = []
                  Action = MapState (fun role -> Solution.build semverInfoOp role.Solution; none) }

            | Msg.Test ->
                { PreviousMsgs = [ Msg.Build None ]
                  Action = MapState (fun role -> Solution.test role.Solution; none) }

            | Msg.Zip projects ->
                { PreviousMsgs = [ Msg.Build None ]
                  Action = MapState (fun role -> 
                    projects |> List.collect (fun project ->
                        project.OutputPaths |> List.map (fun outputPath ->
                            let dir = Path.getDirectory outputPath
                            let zipName = project.Name + ".zip"
                            !! (dir </> "**")
                            |> Zip.zip dir zipName
                            dir </> zipName
                        ) 
                    )
                    |> box ) }
        )