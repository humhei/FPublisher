namespace FPublisher.Roles
open FPublisher
open FPublisher.Utils
open Microsoft.FSharp.Reflection
open System.Threading.Tasks
open Microsoft.FSharp.Quotations
open System.Diagnostics

module Primitives =
        
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
                logger.Important "Start target %s" stateName
                let stopWatch = Stopwatch.StartNew()
                let result = action()
                logger.Important "Finished target %s in %O" stateName stopWatch.Elapsed
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
                FSharpValue.MakeRecord(recordType,values)
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
                
        type RoleActionType<'role,'stateResult,'childTargetState> =
            | MapState of ('role -> 'stateResult)
            | MapChild of ('role -> 'childTargetState)

        type RoleAction<'role,'msg,'stateResult,'childTargetState> =
            { PreviousMsgs: 'msg list 
              Action: RoleActionType<'role,'stateResult,'childTargetState> }

        [<RequireQualifiedAccess>]            
        module Role =
            let rec updateComplex (makeRoleAction: 'role -> 'msg -> RoleAction<'role,'msg,'stateResult,'childTargetState>) (msg: 'msg) (role: 'role when 'role :> IRole<'TargetState>) =
                
                let targetState: 'TargetState =
                    let tp = typeof<'role>
                    tp.GetProperty("TargetState").GetValue(role)
                    |> unbox 
                    
                let roleAction = makeRoleAction role msg

                let role = 
                    roleAction.PreviousMsgs |> List.fold (fun role msg ->
                        updateComplex makeRoleAction msg role
                    ) role                  

                let stateName = UnionCase.getName msg 
                match roleAction.Action with 
                | MapState action ->
                    let newTargetState = 
                        TargetState.updateByStateName stateName (fun _ ->
                            box (action role)
                        ) targetState   
                    Record.setProperty "TargetState" (fun _ -> box newTargetState) role

                | MapChild mapping ->
                    Record.setProperty stateName (fun _ -> box (mapping role)) role
                    
                                

            let update (makeRoleAction: 'msg -> RoleAction<'role, 'msg,'stateResult,'childTargetState>) (msg: 'msg) (role: 'role when 'role :> IRole<'TargetState>) =
                updateComplex (fun _ msg -> makeRoleAction msg) msg role
