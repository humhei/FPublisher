namespace FPublisher.Roles
open FPublisher
open FPublisher.Utils
open Microsoft.FSharp.Reflection
open System.Threading.Tasks
open Microsoft.FSharp.Quotations
open Microsoft.Build.Logging
open System

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

    type SimpleState = State<unit> 
    
    [<RequireQualifiedAccess>]
    module SimpleState =
        let update action (state: SimpleState) =
            match state with 
            | SimpleState.Init -> 
                action()
                SimpleState.Result ()
            | SimpleState.Result _ -> state 


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
                Record.setProperty stateName (function 
                    | :? SimpleState as simpleState ->
                        SimpleState.update action simpleState
                        |> box
                    | _ -> failwith "not implemented"         
                ) targetState

            let update (expr: Expr<_>) action (targetState: 'targetState): 'targetState =
                let stateName = Expr.nameof expr
                updateByStateName stateName action targetState
                
        type RoleAction<'role,'msg> =
            { PreviousMsgs: 'msg list 
              Action: 'role -> unit }

        
        [<RequireQualifiedAccess>]            
        module Role =
            let rec update (makeRoleAction: 'msg -> RoleAction<'role,'msg>) (msg: 'msg) (role: 'role when 'role :> IRole<'TargetState>) =
                let targetState: 'TargetState =
                    let tp = typeof<'role>
                    tp.GetProperty("TargetState").GetValue(role)
                    |> unbox 
                    
                let roleAction = makeRoleAction msg

                let role = 
                    roleAction.PreviousMsgs |> List.fold (fun role msg ->
                        update makeRoleAction msg role
                    ) role                  

                let newTargetState = 
                    let stateName = UnionCase.getName msg 
                    TargetState.updateByStateName stateName (fun _ ->
                        roleAction.Action role
                    ) targetState               
                                
                Record.setProperty "TargetState" (fun _ -> box newTargetState) role
