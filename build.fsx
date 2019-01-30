#if FAKE
#r "paket: groupref Main //"
#endif
#if !FAKE
#r "netstandard" // windows

#endif
#load "./.fake/build.fsx/intellisense.fsx"

open Fake.DotNet.NuGet
open Fake.Core

let result = Version.getLastNuGetVersion "https://www.nuget.org/api/v2" "FcsWatch"
let version = "0.1.4-beta001"
let semver = SemVer.parse version
semver.PreRelease