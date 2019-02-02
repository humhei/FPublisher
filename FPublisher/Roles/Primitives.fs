namespace FPublisher.Roles
open FPublisher
open FPublisher.Utils
module private Primitives =
        

    type IRole<'Target> =
        abstract member NickStatus: 'Target
        abstract member Run: 'Target -> IRole<'Target>

    
    type IIParentRole<'Target> =
        inherit IRole<'Target>
        abstract WithStatus: 'Target -> IIParentRole<'Target>
        abstract WithChild : IRole<'ChildTarget> -> IIParentRole<'Target>    

    type IParentRole<'Target, 'ChildTarget> =
        inherit IRole<'Target>
        abstract Child: IRole<'ChildTarget>
        abstract WithStatus: 'Target -> IParentRole<'Target, 'ChildTarget>
        abstract WithChild : IRole<'ChildTarget> -> IParentRole<'Target, 'ChildTarget>


    /// child status can be matched automaticlly
    let private matchChild (role: IParentRole<'Target,'ChildTarget>) (status: 'Target) matchExpr =
        if UnionCase.isFirst status 
        then
            let child = 
                let childMsg = UnionCase.createByIndex<'ChildTarget> 0
                role.Child.Run childMsg

            if UnionCase.isLast child.NickStatus then 
                let secondStatus = UnionCase.createByIndex<'Target> 1
                role
                    .WithChild(child)
                    .WithStatus secondStatus
                :> IRole<'Target>                
            else 
                let firstStatus = UnionCase.createByIndex<'Target> 0
                role
                    .WithChild(child)
                    .WithStatus firstStatus
                :> IRole<'Target>                
        else matchExpr status 

    let run (msg: 'target) (role: IParentRole<'target,'childTarget>) matchExpr =
        let rec loop msg (role: IParentRole<'target,'childTarget>) =
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
                | :? IParentRole<'target,_> as parentRole -> 
                    matchChild parentRole status matchExpr
                | _ ->  matchExpr status
            else 
                role       
      
        loop msg role      

    let run (msg: 'target) (role: IRole<'target>) matchExpr =
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
                | :? IParentRole<'target,_> as parentRole -> 
                    matchChild parentRole status matchExpr
                | _ ->  matchExpr status
            else 
                role       
      
        loop msg role      