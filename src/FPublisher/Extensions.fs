namespace FPublisher

open Fake.IO
open System.IO
open Fake.IO.FileSystemOperators

[<AutoOpen>]
module internal InternalExtensions =

    [<RequireQualifiedAccess>]
    module Path =
        let normalizeToUnixCompatible path =
            let path = (Path.getFullName path).Replace('\\','/')

            let dir = Path.getDirectory path

            let segaments =
                let fileName = Path.GetFileName path
                fileName.Split([|'\\'; '/'|])

            let folder dir segament =
                dir </> segament
                |> Path.getFullName

            segaments
            |> Array.fold folder dir

    type System.String with 
        member x.Contains(text: string, ignoreCase) = 
            if ignoreCase then x.ToLower().Contains(text.ToLower())
            else x.Contains(text)