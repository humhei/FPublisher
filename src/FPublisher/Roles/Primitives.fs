namespace FPublisher.Roles
open FPublisher
open FPublisher.Utils
open Microsoft.FSharp.Reflection
open System.Threading.Tasks
open Microsoft.FSharp.Quotations
open System.Diagnostics
open FPublisher.FakeHelper.Build
open Fake.Core

[<AutoOpen>]
module internal Global =
    let mutable logger = Logger.create (Logger.Level.Minimal)


module Primitives =

    type Logger.Logger with
        member x.CurrentVersion (currentVersionOp: SemVerInfo option) =
            match currentVersionOp with
            | Some currentVersion ->
                logger.Important "Current version is %s" (SemVerInfo.normalize currentVersion)
            | None ->
                logger.Important "Current version is None"

    type IRole<'TargetState> = interface end

    [<RequireQualifiedAccess>]
    type StateTask<'Result> =
        | Init
        | Result of Task<'Result>


    [<RequireQualifiedAccess>]
    type State<'Result> =
        | Init
        | Result of 'Result

    type BoxedState = State<obj>



    let none = box ()

    [<RequireQualifiedAccess>]
    module State =
        let update stateName action (state: BoxedState) =
            match state with
            | State.Init ->
                logger.Important "FPUBLISH: Start target %s" stateName
                let stopWatch = Stopwatch.StartNew()
                let result = action()
                logger.Important "FPUBLISH: Finished target %s in %O" stateName stopWatch.Elapsed
                State.Result result
            | State.Result _ -> state

        let getResult = function
            | State.Init -> failwith "result have not been evaluated"
            | State.Result result -> result |> unbox


    [<AutoOpen>]
    module internal Reflection =

        [<RequireQualifiedAccess>]
        module Record =
            let setProperty name makeValue (record: 'record) : 'record =
                let recordType = typeof<'record>

                let values =
                    FSharpType.GetRecordFields(recordType)
                    |> Array.map (fun prop ->
                        let value = prop.GetValue(record)

                        if prop.Name = name
                        then makeValue value
                        else value
                    )
                FSharpValue.MakeRecord(recordType, values)
                |> unbox


        [<RequireQualifiedAccess>]
        module TargetState =
            let updateByStateName stateName action (targetState: 'targetState): 'targetState =
                Record.setProperty stateName (fun value ->
                    State.update stateName action (unbox value)
                    |> box
                ) targetState

            let update (expr: Expr<_>) action (targetState: 'targetState): 'targetState =
                let stateName = Expr.nameof expr
                updateByStateName stateName action targetState

        type RoleActionType<'role,'stateResult,'childRole> =
            | MapState of ('role -> 'stateResult)
            | MapChild of ('role -> 'childRole)

        type RoleAction<'role,'msg,'stateResult,'childRole> =
            { PreviousMsgs: 'msg list
              Action: RoleActionType<'role,'stateResult,'childRole> }

        [<RequireQualifiedAccess>]
        module Role =

            let rec updateComplex (makeRoleAction: 'role -> 'msg -> RoleAction<'role,'msg,'stateResult,'childRole>) (msg: 'msg) (role: 'role when 'role :> IRole<'TargetState>) =



                let roleAction = makeRoleAction role msg

                let role =
                    roleAction.PreviousMsgs |> List.fold (fun role msg ->
                        updateComplex makeRoleAction msg role
                    ) role

                let targetState: 'TargetState =
                    let tp = typeof<'role>
                    tp.GetProperty("TargetState").GetValue(role)
                    |> unbox

                let stateName = UnionCase.getName msg
                match roleAction.Action with
                | MapState action ->
                    let newTargetState =
                        TargetState.updateByStateName stateName (fun _ ->
                            box (action role)
                        ) targetState
                    Record.setProperty "TargetState" (fun _ -> box newTargetState) role

                | MapChild mapping ->
                    let newChildRole = mapping role
                    let newChildTargetState = newChildRole.GetType().GetProperty("TargetState").GetValue(newChildRole)

                    let setChildRole role = Record.setProperty stateName (fun _ -> box newChildRole) role
                    let setChildTargetState role =
                        let newTargetState = Record.setProperty stateName (fun _ -> box newChildTargetState ) targetState
                        Record.setProperty "TargetState" (fun _ -> box newTargetState) role

                    role |> setChildRole |> setChildTargetState



            let update (makeRoleAction: 'msg -> RoleAction<'role, 'msg,'stateResult,'childRole>) (msg: 'msg) (role: 'role when 'role :> IRole<'TargetState>) =
                updateComplex (fun _ msg -> makeRoleAction msg) msg role
