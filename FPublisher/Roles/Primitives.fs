namespace FPublisher.Roles
open FPublisher
open FPublisher.Utils
open Microsoft.FSharp.Reflection

module private Primitives =
        

    type IRole<'Target> =
        abstract member NickStatus: 'Target
        abstract member Run: 'Target -> IRole<'Target>
  

    type IIParentRole = interface end

    type IParentRole<'Target, 'ChildTarget> =
        inherit IIParentRole
        inherit IRole<'Target>
        abstract Child: IRole<'ChildTarget>
        abstract WithStatus: 'Target -> IParentRole<'Target, 'ChildTarget>
        abstract WithChild : IRole<'ChildTarget> -> IParentRole<'Target, 'ChildTarget>



    let private runCommon (msg: 'target) (role: IRole<'target>) matchChild matchExpr =
        let rec loop msg (role: IRole<'target>) =
            let msgIndex = UnionCase.findIndex msg
            
            let status = role.NickStatus

            let statusIndex = UnionCase.findIndex status

            if msgIndex > statusIndex 
            then
                let previousMsg = UnionCase.createByIndex (msgIndex - 1)            
                loop previousMsg role   
            elif msgIndex = statusIndex 
            then 
                match role with 
                | :? IIParentRole -> matchChild status
                | _ -> matchExpr status
            else 
                role       
      
        loop msg role      

    let run (msg: 'target) (role: IRole<'target>) matchExpr =
        runCommon msg role matchExpr matchExpr


    /// child status can be matched automaticlly
    let runParent msg (role: IParentRole<'Target,'ChildTarget>) matchExpr =
        runCommon msg role (fun status ->
            if UnionCase.isFirst status 
            then
                let child = 
                    let _,fields = FSharpValue.GetUnionFields(status,typeof<'Target>)
                    role.Child.Run (unbox fields.[0])

                if UnionCase.isLast child.NickStatus then 
                    let secondStatus = UnionCase.createByIndex<'Target> 1
                    role
                        .WithChild(child)
                        .WithStatus secondStatus
                    :> IRole<'Target>                
                else 
                    let firstStatus = 
                        UnionCase.createByIndexWith 0 [|box child.NickStatus|]
                    role
                        .WithChild(child)
                        .WithStatus firstStatus
                    :> IRole<'Target>     
            else matchExpr status                  
        ) matchExpr
             

