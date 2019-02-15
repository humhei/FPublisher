namespace FPublisher
open Fake.Core

[<RequireQualifiedAccess>]
module Logger = 
    open System

    [<RequireQualifiedAccess>]
    type Level =
        | Minimal
        | Normal
        | Quiet

    type Logger internal (level) =
        
        let timeStamp (time:DateTime) = time.ToString("yyyy-MM-dd HH:mm:ss.fff")

        let withTimeStamp (f: string -> unit) =
            fun message ->
                let now = timeStamp DateTime.Now
                sprintf "%s %s" now message 
                |> f


        let _info message =
            match level with 
            | Level.Minimal -> ()
            | Level.Normal -> Trace.log message
            | Level.Quiet -> ()


        let _important message =
            match level with 
            | Level.Quiet -> ()
            | _ -> Trace.trace message  

        member x.Info format =
            Printf.ksprintf _info format  


        member x.Diagnostics text =
            System.Diagnostics.Debugger.Log(1,"",sprintf "%s %s\n" (timeStamp DateTime.UtcNow) text)

        /// with timeStamp
        member x.Infots format =
            Printf.ksprintf (withTimeStamp _info) format


        member x.Important format =
            Printf.ksprintf _important format   

        member x.Importantts format =
            Printf.ksprintf (withTimeStamp _important) format   

        member x.Warn message =
            Trace.traceImportant message  
              
        member x.Error message =
            Trace.traceError message       

    let create level = Logger(level)


