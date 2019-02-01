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
let collection = Regex.Matches(line,pattern,RegexOptions.ExplicitCapture)
let s = collection.[0]
let rec parseAll line =
    match line with 
    | ParseRegex pattern (s::a::_) -> 
        s,a
    | other -> failwith "Hello"


let projectHex = hex <|> anyOf ['-';'{';'}';'\"'] 
let parser: Parser<_,unit> = pstring "Project(\"{" >>. many1Chars projectHex >>. pstring "}\""

let s = run parser line
let readProjects (slnPath: string) = 
    File.readWithEncoding Encoding.UTF8 slnPath
    |> List.ofSeq

let text = readProjects @"D:\VsCode\Github\FPublisher\FPublisher.FPublisher.sln"


let result = Version.getLastNuGetVersion "https://www.nuget.org/api/v2" "FcsWatch"
let version = "0.1.4-beta001"
let semver = SemVer.parse version
semver.PreRelease