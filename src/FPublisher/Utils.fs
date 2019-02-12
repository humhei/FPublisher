namespace FPublisher
open Fake.IO
open Fake.DotNet
open Fake.Core
open Microsoft.FSharp.Quotations
open System.Collections.Concurrent
open Microsoft.FSharp.Reflection
open System.Threading.Tasks

module internal Utils = 

    [<RequireQualifiedAccess>]
    module Task =
        let getResult (task: Task<_>) =
            task.Result

    [<RequireQualifiedAccess>]
    module internal UnionCase =
        let private cache = new ConcurrentDictionary<System.Type,UnionCaseInfo []>()

        let getUnionCaseInfos (tp: System.Type) =
            cache.GetOrAdd(tp,FSharpType.GetUnionCases)

        let createByIndexWith<'UnionCaseType> index values  : 'UnionCaseType =
            let unionCaseInfo =
                getUnionCaseInfos typeof<'UnionCaseType> 
                |> Array.item index

            FSharpValue.MakeUnion(unionCaseInfo,values)
            |> unbox
        
        let createByIndex<'UnionCaseType> index : 'UnionCaseType =
            createByIndexWith index [||]

        let getName (unionCase: 'UnionCaseType) =
            let uci = 
                FSharpValue.GetUnionFields(unionCase,typeof<'UnionCaseType>)
                |> fst  
            uci.Name            
                      

        let findIndex (unionCase: 'UnionCaseType) =
            let unionCases = getUnionCaseInfos typeof<'UnionCaseType>

            let currentUnionCase = 
                FSharpValue.GetUnionFields(unionCase,typeof<'UnionCaseType>)
                |> fst        

            unionCases
            |> Array.findIndex (fun unionCase -> unionCase.Name =  currentUnionCase.Name)

        let isLast (unionCase: 'UnionCaseType) =
            let index = findIndex unionCase
            let unionCaseInfos = getUnionCaseInfos typeof<'UnionCaseType>
            unionCaseInfos.Length - 1 = index


        let isFirst (unionCase: 'UnionCaseType) =
            findIndex unionCase = 0              

    let inline dtntSmpl arg = DotNet.Options.lift id arg
    
                               


    [<RequireQualifiedAccess>]
    module Expr =
        let nameof (q:Expr<_>) = 
            match q with 
            | Patterns.Let(_, _, DerivedPatterns.Lambdas(_, Patterns.Call(_, mi, _))) -> mi.Name
            | Patterns.PropertyGet(_, mi, _) -> mi.Name
            | DerivedPatterns.Lambdas(_, Patterns.Call(_, mi, _)) -> mi.Name
            | DerivedPatterns.Lambdas(_, Patterns.NewUnionCase(uc,_)) -> uc.Name
            | _ -> failwith "Unexpected format"    

    [<RequireQualifiedAccess>]
    module String =
        open System
        let ofCharList chars = chars |> List.toArray |> String
        
        let equalIgnoreCaseAndEdgeSpace (text1: string) (text2: string) = 
            let trimedText1 = text1.Trim()
            let trimedText2 = text2.Trim()

            String.Equals(trimedText1,trimedText2,StringComparison.InvariantCultureIgnoreCase)

    [<RequireQualifiedAccess>]
    module Seq =

        let mapFindedFirst prediate mapping values =
            let index = Seq.findIndex prediate values 
            values 
            |> Seq.indexed 
            |> Seq.collect (fun (i,value) -> if i = index then mapping value else [value])



    [<RequireQualifiedAccess>]
    module File =

        let replaceFindedFirstLine (finder: string -> bool) mapping (file: string) =
            let lines = File.readWithEncoding System.Text.Encoding.UTF8 file |> List.ofSeq
            lines |> Seq.mapFindedFirst finder mapping
            |> File.writeWithEncoding System.Text.Encoding.UTF8 false file 
        