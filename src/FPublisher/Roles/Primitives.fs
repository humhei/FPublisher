﻿namespace FPublisher.Roles

open Microsoft.FSharp.Reflection
open FPublisher
open System.Diagnostics
open Fake.Core
open FPublisher.FakeHelper.Build

module Primitives =

    type Logger.Logger with
        member x.CurrentVersion (currentVersionOp: SemVerInfo option) =
            match currentVersionOp with
            | Some currentVersion ->
                logger.ImportantGreen "Current version is %s" (SemVerInfo.normalize currentVersion)
            | None ->
                logger.ImportantGreen "Current version is None"

    let none = box ()

    type ITargetState =
        abstract member IsInit: bool
        abstract member IsDone: bool

    [<RequireQualifiedAccess>]
    type TargetState<'result> =
        | Init
        | Done of 'result
    with 
        interface ITargetState with 
            member x.IsDone =
                match x with 
                | Done v -> true
                | Init -> false

            member x.IsInit =
                match x with 
                | Done v -> false
                | Init -> true


    type BoxedTargetState = TargetState<obj>

    module TargetState =
        let update stateName action (state: BoxedTargetState) =
            match state with
            | TargetState.Init ->
                logger.ImportantGreen "FPUBLISH: Start target %s" stateName
                let stopWatch = Stopwatch.StartNew()
                let result = action()
                logger.ImportantGreen "FPUBLISH: Finished target %s in %O" stateName stopWatch.Elapsed
                TargetState.Done result
            | TargetState.Done _ -> state

        let getResult = function
            | TargetState.Init -> failwith "result have not been evaluated"
            | TargetState.Done result -> unbox result

    type RoleAction<'role> =
        | MapState of ('role -> obj)
        | MapChild of ('role -> 'role)

    type RoleIntegratedAction<'role, 'target> =
        { DependsOn: 'target list
          Action: RoleAction<'role> }

    type IRole<'targetStates> = interface end

    [<AutoOpen>]
    module IRoleExtensions =

        [<RequireQualifiedAccess>]
        module private Record =

            /// immutable
            let setProperty name newValue (record: 'record) : 'record =
                let recordType = typeof<'record>

                let values =
                    FSharpType.GetRecordFields(recordType)
                    |> Array.map (fun prop ->
                        let value = prop.GetValue(record)

                        if prop.Name = name
                        then newValue
                        else value
                    )
                FSharpValue.MakeRecord(recordType, values)
                |> unbox

        type IRole<'targetStates> with  
            static member Run(makeRoleIntegratedAction: 'role -> 'target -> RoleIntegratedAction<'role, 'target>) =
                fun
                    (target: 'target) 
                    (role: 'role when 'role :> IRole<'targetStates>) ->

                let integratedAction = makeRoleIntegratedAction role target

                let role =
                    (role,integratedAction.DependsOn) ||> List.fold (fun role target ->
                        IRole<'targetStates>.Run makeRoleIntegratedAction target role
                    ) 

                let targetStates: 'targetStates =
                    let tp = typeof<'role>
                    tp.GetProperty("TargetStates").GetValue(role)
                    |> unbox

                let targetName = 
                    let tp = target.GetType()
                    let uci, _ = FSharpValue.GetUnionFields(target, tp)
                    uci.Name
                    

                match integratedAction.Action with 
                | MapState mapping ->
                    let targetStateProperty = 
                        FSharpType.GetRecordFields(typeof<'targetStates>) 
                        |> Array.find (fun prop -> prop.Name = targetName)
                        |> fun prop -> prop

                    let targetStatePropertyType = targetStateProperty.PropertyType

                    let targetState = 
                        targetStateProperty.GetValue(targetStates) :?> ITargetState

                    match targetState.IsDone with 
                    | true -> role
                    | false -> 

                        let newTargetStates = 
                            logger.ImportantGreen "FPUBLISH: Start target %s" targetName
                            let stopWatch = Stopwatch.StartNew()

                            let newTargetState = 


                                let uci = 
                                    FSharpType.GetUnionCases(targetStatePropertyType)
                                    |> Array.find (fun uci -> uci.Name = "Done")
                                FSharpValue.MakeUnion(uci, [| mapping role |])

                            logger.ImportantGreen "FPUBLISH: Finished target %s in %O" targetName stopWatch.Elapsed

                            Record.setProperty targetName newTargetState targetStates

                        Record.setProperty "TargetStates" newTargetStates role

                | MapChild mapping ->
                    let newRole = mapping role
                    newRole
                    //let newTargetStates = 
                    //    let newChildTargetStates = newChildRole.GetType().GetProperty("TargetStates").GetValue(newChildRole)
                    //    Record.setProperty targetName newChildTargetStates targetStates

                    //Record.setProperty "TargetStates" newTargetStates role

            static member Run(makeRoleIntegratedAction: 'target -> RoleIntegratedAction<'role, 'target>) = 
                IRole<'targetStates>.Run(fun _ -> makeRoleIntegratedAction)

