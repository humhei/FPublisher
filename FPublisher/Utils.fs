namespace FPublisher
open FParsec
open Fake.IO
open Fake.DotNet
open Fake.Core
module Utils = 
    let inline dtntSmpl arg = DotNet.Options.lift id arg
    
    [<RequireQualifiedAccess>]
    type Logger =
        | Minimal
        | Normal
        | Quiet

    [<RequireQualifiedAccess>]
    module Logger = 
        open System

        let mutable private logger = Logger.Minimal
        let internal setDefaultLogger newLogger = logger <- newLogger

        let private timeStamp (time:DateTime) = time.ToString("yyyy-MM-dd HH:mm:ss.fff")
        let diagnostics text =
            System.Diagnostics.Debugger.Log(1,"",sprintf "%s %s\n" (timeStamp DateTime.UtcNow) text)


        let private _info message =
            match logger with 
            | Logger.Minimal -> ()
            | Logger.Normal -> Trace.log message
            | Logger.Quiet -> ()

        let info format =
            Printf.ksprintf _info format

        let private withTimeStamp (f: string -> unit) =
            fun message ->
                let now = timeStamp DateTime.Now
                sprintf "%s %s" now message 
                |> f

        /// with timeStamp
        let infots format =
            Printf.ksprintf (withTimeStamp _info) format
            

        let writelog (s:string) = printfn "LOG: %s" s
        let writelogf fmt = Printf.kprintf writelog fmt



        let private _important message =
            match logger with 
            | Logger.Quiet -> ()
            | _ -> Trace.trace message  
        
        let important format =
            Printf.ksprintf _important format   

        let importantts format =
            Printf.ksprintf (withTimeStamp _important) format           

        let warn message =
            Trace.traceImportant message  
              
        let error message =
            Trace.traceError message        

    [<AutoOpen>]
    module FParsec =

        let inline (+>>+) parser1 parser2 =
            (parser1 |>> string) .>>. (parser2 |>> string)
            |>> fun (r1,r2) -> r1 + r2

        let runWithInputReturnOp parsers input =
            match run parsers input with 
            | ParserResult.Success (result,_,_) -> Some input
            | ParserResult.Failure (error,_,_) -> None

        let runWithInputReturn parsers input =
            match run parsers input with 
            | ParserResult.Success (result,_,_) -> input
            | ParserResult.Failure (error,_,_) -> failwithf "%s" error

        let runToResultOp p s = 
            run p s 
            |> function
                | Success (r,_,_) -> Some r
                | Failure (_msg,_error,_) -> None

        let runToResult p s = 
            run p s 
            |> function
                | Success (r,_,_) -> r
                | Failure (error,_,_) -> failwithf "%s" error
        
        let runToBoolen p s = 
            run p s 
            |> function
                | Success (r,_,_) -> true
                | Failure (error,_,_) -> false

    [<RequireQualifiedAccess>]
    module String =
        open System
        let ofCharList chars = chars |> List.toArray |> String
        
        let equalIgnoreCaseAndEdgeSpace (text1: string) (text2: string) = 
            match text1.Trim().CompareTo (text2.Trim()) with 
            | 0 -> true
            | _ -> false

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
        