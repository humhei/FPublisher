namespace FPublisher
open Fake.Core
open Fake.DotNet
open Fake.Tools.Git.CommandHelper
open Fake.Tools.Git
open Microsoft.FSharp.Core.Operators
open Fake.IO
open Utils
open System
open Fake.DotNet.NuGet
module FakeHelper =


    [<RequireQualifiedAccess>]
    module ProcessResult =
        let ensureExitCode (processResult: ProcessResult) =
            if processResult.ExitCode <> 0
            then
                failwithf "%A" processResult.Errors

    module CommandHelper =

        let platformTool tool =
            tool
            |> ProcessUtils.tryFindFileOnPath
            |> function Some t -> t | _ -> failwithf "%s not found" tool



        let dotnetWith dir command args =
            DotNet.exec
                (fun ops -> {ops with WorkingDirectory = dir})
                command
                (Args.toWindowsCommandLine args)

        let dotnet dir command args =
            dotnetWith dir command args
            |> ProcessResult.ensureExitCode

        let dotnetGlobalTool tool =
            tool
            |> ProcessUtils.tryFindFileOnPath
            |> function 
                | Some t -> t 
                | None -> 
                    dotnet "./" "tool" ["install"; "-g"; tool] 
                    platformTool tool

        let exec tool dir args =
            let result =
                args
                |> CreateProcess.fromRawCommand tool
                |> CreateProcess.withWorkingDirectory dir
                |> Proc.run

            if result.ExitCode <> 0
            then failwithf "Error while running %s with args %A" tool (List.ofSeq args)

        let git args dir = exec "git" dir args
        [<RequireQualifiedAccess>]
        module Git =
            let private msgs (ok,msgs,error) = msgs

            /// get stagedFileLines
            let stagedFileLines dir =
                runGitCommand dir "diff --name-only --cached"
                |> msgs

            /// stageAll and then run diff
            let diff dir =
                Staging.stageAll dir
                stagedFileLines dir


    [<RequireQualifiedAccess>]
    module Directory =
        /// ensure dirctory and return it
        let ensureReturn directory =
            Directory.ensure directory
            directory

    module Build =


        /// Compatible SemVerInfo module for nuget
        [<RequireQualifiedAccess>]
        module internal SemVerInfo =
            let private alphaLiterals =
                [ "alpha"
                  "rc"
                  "beta" ]


            let internal normalize (semverInfo: SemVerInfo) =
                let build =
                    if semverInfo.Build > 0I then ("." + semverInfo.Build.ToString("D")) else ""

                let pre =
                    match semverInfo.PreRelease with
                    | Some preRelease -> ("-" + preRelease.Origin)
                    | None -> ""

                sprintf "%d.%d.%d%s%s" semverInfo.Major semverInfo.Minor semverInfo.Patch pre build




            let mainVersionText (semverInfo: SemVerInfo) =
                sprintf "%d.%d.%d" semverInfo.Major semverInfo.Minor semverInfo.Patch



            /// 0.1.4-alpha.0.1 -> 0.1.4-alpha.0+1
            let normalizeBuild (semVerInfo: SemVerInfo) =
                match semVerInfo.PreRelease with
                | Some prelease ->
                    match prelease.Values with
                    | [segment1; segment2; segment3] ->
                        match (segment1,segment2,segment3) with
                        | ( PreReleaseSegment.AlphaNumeric alpha,
                            PreReleaseSegment.Numeric num,
                            PreReleaseSegment.Numeric buildNum ) ->
                            let v = sprintf "%s-%s.%O+%O" (mainVersionText semVerInfo) alpha num buildNum |> SemVer.parse
                            { v with Build = buildNum }

                        | _ -> semVerInfo
                    | _ -> semVerInfo

                | None -> semVerInfo

            let internal parse text =
                SemVer.parse text
                |> normalizeBuild

            /// 0.1.4-alpha -> 0.1.4-alpha.0
            let normalizeAlpha (semverInfo: SemVerInfo) =
                match semverInfo.PreRelease with
                | Some preRelease ->
                    match preRelease.Values with
                    | [segment] ->
                        match segment with
                        | PreReleaseSegment.AlphaNumeric text ->
                            if List.contains text alphaLiterals
                            then
                                sprintf "%s-%s.0" (mainVersionText semverInfo) text
                                |> parse
                            else semverInfo
                        | _ -> semverInfo
                    | _ -> semverInfo
                | None -> semverInfo

            let nextBuildVersion (semverInfo: SemVerInfo) =
                let semverInfo = normalizeAlpha semverInfo

                match semverInfo.PreRelease with
                | Some prerelease ->
                    {semverInfo with Build = semverInfo.Build + 1I}
                    |> normalize
                    |> parse
                | None ->
                    let build = (semverInfo.Build + 1I).ToString("D")
                    sprintf "%s-alpha.0.%s" (mainVersionText semverInfo) build
                    |> parse

            let nextBetaVersion (semverInfo: SemVerInfo) =
                let semverInfo = normalizeAlpha semverInfo

                let newVersionText =
                    match semverInfo.PreRelease with
                    | Some prelease ->
                        match prelease.Values with
                        | [segment1;segment2] ->
                            match segment1,segment2 with
                            | PreReleaseSegment.AlphaNumeric alpha, PreReleaseSegment.Numeric num ->
                                sprintf "%s-%s.%s" (mainVersionText semverInfo) alpha ((num + 1I).ToString())
                            | _ -> failwith "not implementd"
                        | _ -> failwith "not implemented"
                    | None ->
                        let nextPatchVersion = { Version.IncPatch semverInfo with PreRelease = None }
                        sprintf "%s-%s" (normalize nextPatchVersion) "alpha.1"

                parse newVersionText



        [<RequireQualifiedAccess>]
        module ReleaseNotes =
            let private tbdHeaderLine (releaseNotes: ReleaseNotes.ReleaseNotes) =
                match releaseNotes.SemVer.PreRelease with
                | Some prelease -> sprintf "## %s-%s - tbd" (SemVerInfo.mainVersionText releaseNotes.SemVer) prelease.Name
                | None ->
                    let newPatchVersion = { Version.IncPatch releaseNotes.SemVer with PreRelease = None }
                    sprintf "## %s-alpha - tbd" (SemVerInfo.normalize newPatchVersion)

            let private todayHeaderLine (releaseNotes: ReleaseNotes.ReleaseNotes) =
                sprintf "## %s - %s" (SemVerInfo.normalize releaseNotes.SemVer) (DateTime.Now.ToString("yyyy-MM-dd"))

            let updateDateToToday (releaseNotes: ReleaseNotes.ReleaseNotes) =
                { releaseNotes with Date = Some DateTime.Today }

            let updateWithSemVerInfo (semverInfo: SemVerInfo) (releaseNotes: ReleaseNotes.ReleaseNotes) =
                { releaseNotes with
                    AssemblyVersion = SemVerInfo.mainVersionText semverInfo
                    NugetVersion = SemVerInfo.normalize semverInfo
                    SemVer = semverInfo }

            /// replace next version date tbd to today date
            /// and add next next version with tbd
            let writeToNext file (releaseNotes: ReleaseNotes.ReleaseNotes) =
                File.replaceFindedFirstLine
                    (fun line -> line.TrimEnd() |> String.endsWith "tbd")
                    (fun _ ->
                        [ yield tbdHeaderLine releaseNotes
                          yield ""
                          yield todayHeaderLine releaseNotes])
                    file

            /// load Release_Notes who's date is -tbd
            /// ## 0.1.0-beta - tbd
            let loadTbd (file: string) =
                let lines = File.readWithEncoding System.Text.Encoding.UTF8 file

                lines
                |> ReleaseNotes.parseAll
                |> List.filter (fun releaseNotes -> releaseNotes.Date = None)
                |> function
                    | [tbdReleaseNotes] -> tbdReleaseNotes
                    | _ -> failwith "Cannot find a tbd release notes"


            let loadLast (file: string) =
                let lines = File.readWithEncoding System.Text.Encoding.UTF8 file

                lines
                |> ReleaseNotes.parseAll
                |> List.filter (fun releaseNotes -> releaseNotes.Date <> None)
                |> function
                    | values when values.Length > 0 ->
                        values
                        |> List.maxBy (fun releaseNotes -> releaseNotes.Date, releaseNotes.SemVer)
                        |> Some

                    | _ -> None
