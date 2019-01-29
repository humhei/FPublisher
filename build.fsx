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
