namespace FPublisher.Roles

open Microsoft.FSharp.Reflection
open FPublisher
open System.Diagnostics

module Primitives =

    let none = box ()

    [<RequireQualifiedAccess>]
    type TargetState<'result> =
        | Init
        | Done of 'result

    type BoxedTargetState = TargetState<obj>

    type RoleAction<'role, 'childRole> =
        | MapState of ('role -> obj)
        | MapChild of ('role -> 'childRole)

    type RoleIntegratedAction<'role, 'target, 'childRole> =
        { DependsOn: 'target list
          Action: RoleAction<'role, 'childRole> }

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
            static member Run(makeRoleIntegratedAction: 'role -> 'target -> RoleIntegratedAction<'role, 'target, 'childRole>) =
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
                    let newTargetStates = 
                        logger.ImportantGreen "FPUBLISH: Start target %s" targetName
                        let stopWatch = Stopwatch.StartNew()

                        let newTargetState = 
                            let targetStatePropertyType = 
                                FSharpType.GetRecordFields(typeof<'targetStates>) 
                                |> Array.find (fun prop -> prop.Name = targetName)
                                |> fun prop -> prop.PropertyType

                            let uci = 
                                FSharpType.GetUnionCases(targetStatePropertyType)
                                |> Array.find (fun uci -> uci.Name = "Done")
                            FSharpValue.MakeUnion(uci, [| mapping role |])

                        logger.ImportantGreen "FPUBLISH: Finished target %s in %O" targetName stopWatch.Elapsed

                        Record.setProperty targetName newTargetState targetStates

                    Record.setProperty "TargetStates" newTargetStates role

                | MapChild mapping ->
                    let newTargetStates = 
                        let newChildRole = mapping role
                        let newChildTargetStates = newChildRole.GetType().GetProperty("TargetStates").GetValue(newChildRole)
                        Record.setProperty targetName newChildTargetStates targetStates

                    Record.setProperty "TargetStates" newTargetStates role

            static member Run(makeRoleIntegratedAction: 'target -> RoleIntegratedAction<'role, 'target, 'childRole>) = 
                IRole<'targetStates>.Run(fun _ -> makeRoleIntegratedAction)