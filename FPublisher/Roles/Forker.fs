namespace FPublisher.Roles
open FPublisher
open FPublisher.Nuget
open Fake.IO.FileSystemOperators
open FPublisher.Git
open Primitives


[<RequireQualifiedAccess>]
module Forker =

    [<RequireQualifiedAccess>]
    type Msg =
        | Build 
        | Test

    type TargetState =
        { Build: SimpleState
          Test: SimpleState }

    [<RequireQualifiedAccess>]
    module TargetState =
        let init =
            { Build = SimpleState.Init 
              Test = SimpleState.Init }

    type Role =
        { Solution: Solution 
          Workspace: Workspace
          TargetState: TargetState }
    with 
        interface IRole<TargetState>
            
    let create (workspace: Workspace) = 

        let slnPath = 
            let workingDir = workspace.WorkingDir
            workingDir </> ("FPublisher." + workspace.DefaultSlnName + ".sln")

        Workspace.createSlnWith slnPath false workspace
                
        { Solution = Solution.read slnPath   
          Workspace = workspace
          TargetState = TargetState.init }
        
    let internal roleAction = function
        | Msg.Build ->
            { PreviousMsgs = []
              Action = fun role -> Solution.buildFail role.Solution } 
        | Msg.Test -> 
            { PreviousMsgs = [Msg.Build]
              Action = fun role -> Solution.testFail role.Solution } 

    let run =
        Role.update roleAction