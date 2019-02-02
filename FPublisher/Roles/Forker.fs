namespace FPublisher.Roles
open FPublisher
open FPublisher.Nuget
open Fake.IO.FileSystemOperators
open FPublisher.Git
open Primitives


[<RequireQualifiedAccess>]
module Forker =

    [<RequireQualifiedAccess>]
    type Target =
        | Init
        | Build 
        | Test


    type Role =
        { Solution: Solution 
          Workspace: Workspace
          Status: Target }
    with 
        interface IRole<Target> with 
            
            member x.NickStatus = x.Status

            member role.Run msg = 
                Primitives.run msg (role :> IRole<Target>) (fun status ->
                    match status with 
                    | Target.Init ->
                        Solution.buildFail role.Solution      
                        { role with
                            Status = Target.Build } 
                    | Target.Build ->
                        Solution.testFail role.Solution      
                        { role with
                            Status = Target.Build } 
                    | Target.Test -> role 
                    :> IRole<Target>  
                )        
            
    let create (workspace: Workspace) =

        let slnPath = 
            let workingDir = workspace.WorkingDir
            workingDir </> ("FPublisher." + workspace.DefaultSlnName + ".sln")

        Workspace.createSlnWith slnPath false workspace

        { Solution = Solution.read slnPath   
          Workspace = workspace
          Status = Target.Init }
        

                                                                                   
                                                                                   
        