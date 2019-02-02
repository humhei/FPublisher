#if FAKE
#r "paket: groupref Main //"
#endif
#if !FAKE
#r "netstandard" // windows

#endif
#load "./.fake/build.fsx/intellisense.fsx"

open Fake.DotNet.NuGet
open Fake.Core
open FParsec
open Fake.IO
open System.Text
open FParsec.CharParsers
open Fake.Core.SemVerActivePattern
open System.Text.RegularExpressions
open FSharp.Literate

let md = """# Markdown is cool
especially with *FSharp.Formatting* ! """
            |> FSharp.Markdown.Markdown.TransformHtml

type ReleaseModel = double
type InitModel = int

[<RequireQualifiedAccess>]
type PublishStatus =
    | None
    | Init of InitModel
    | Build of InitModel
    | RunTest of InitModel
    | EnsureGitChangesAllPushedAndInDefaultBranchBeforeRelease of InitModel
    | WriteReleaseNotesToNextVersionAndPushToRemoteRepositoryWhenRelease of ReleaseModel
    | Packed of ReleaseModel

let keyValuePair =
    let tp =typeof<PublishStatus>
    tp.GetProperties() |> Array.map (fun prop -> prop.Name )


let pattern = "Project[\(\"\{ \}\)\w\-]+\=[ ]+\"(?<name>[\w.]+)\",[ ]+\"(?<relativePath>[\w\\\.]+)\""
