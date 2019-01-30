namespace FPublisher
open Fake.Core
open Fake.DotNet
open Fake.Tools.Git.CommandHelper
open Fake.Tools.Git
open Microsoft.FSharp.Core.Operators
open Fake.IO
open FParsec.CharParsers
open FParsec
open Utils
open System
module FakeHelper =
    module CommandHelper = 

        let toCommandLine args = 
            if Environment.isUnix then Args.toLinuxShellCommandLine args
            else Args.toWindowsCommandLine args

        let platformTool tool =
            tool
            |> ProcessUtils.tryFindFileOnPath
            |> function Some t -> t | _ -> failwithf "%s not found" tool

        let dotnet dir command args =
            DotNet.exec 
                (fun ops -> {ops with WorkingDirectory = dir})
                command
                (toCommandLine args)
                |> ignore    

        let exec tool dir args =
            args
            |> CreateProcess.fromRawCommand tool
            |> CreateProcess.withWorkingDirectory dir
            |> Proc.run
            |> ignore  

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
        [<RequireQualifiedAccess>]
        module PreReleaseSegment =
        
            let incr (preReleaseSegment: PreReleaseSegment) =
                match preReleaseSegment with 
                | PreReleaseSegment.AlphaNumeric text -> 
                    
                    let alpha,number = runToResult (many1Chars asciiLetter .>>. many digit) text

                    match number with 
                    | [] -> alpha + "001"
                    | _ ->
                        let numberString =
                            let maxValue = pown 10 number.Length - 1
                            let numberValue = Int32.Parse (String.ofCharList number)
                            let newNumberValue = numberValue + 1
                            if newNumberValue > maxValue then failwithf "Version can not exceed maxValue %d" maxValue
                            let format = sprintf "D%d" number.Length
                            newNumberValue.ToString(format)
                        alpha + numberString
                    |> PreReleaseSegment.AlphaNumeric
                | PreReleaseSegment.Numeric _ -> failwith "Not implemented" 

        [<RequireQualifiedAccess>]
        module SemVerInfo =
            let normalize (semverInfo: SemVerInfo) =
                let build = if semverInfo.Build > 0I then ("." + semverInfo.Build.ToString("D5")) else ""
                            
                let pre = 
                    match semverInfo.PreRelease with
                    | Some preRelease -> ("-" + preRelease.Origin)
                    | None -> ""

                sprintf "%d.%d.%d%s%s" semverInfo.Major semverInfo.Minor semverInfo.Patch pre build

            let mainVersionText (semverInfo: SemVerInfo) =
                sprintf "%d.%d.%d" semverInfo.Major semverInfo.Minor semverInfo.Patch    

            let nextBuildVersion (semverInfo: SemVerInfo) =
                {semverInfo with Build = semverInfo.Build + 1I}

            let nextPatchVersion (semverInfo: SemVerInfo) =
                sprintf "%d.%d.%d" semverInfo.Major semverInfo.Minor (semverInfo.Patch + 1u)
                |> SemVer.parse 
                
            let nextBetaVersion (semverInfo: SemVerInfo) =
                let newVersionText =         
                    match semverInfo.PreRelease with 
                    | Some prelease ->  
                        let segment = 
                            match prelease.Values with 
                            | [segment] -> PreReleaseSegment.incr segment 
                            | _ -> failwith "not implemented"  
                        
                        match segment with 
                        | PreReleaseSegment.AlphaNumeric origin -> 
                            sprintf "%s-%s" (mainVersionText semverInfo) origin
                        | _ -> failwith "not implemented" 

                    | None -> 
                        let nextPatchVersion = nextPatchVersion semverInfo 
                        sprintf "%s-%s" nextPatchVersion.AsString "alpha001"
                SemVer.parse newVersionText





            let nextMinorVersion (semverInfo: SemVerInfo) =
                { nextPatchVersion semverInfo with 
                    Minor = semverInfo.Minor + 1u }               
        
            let nextMajorVersion (semverInfo: SemVerInfo) =
                { nextMinorVersion semverInfo with 
                    Major = semverInfo.Major + 1u }     

            let nextBetaVersionText (semverInfo: SemVerInfo) =
                let newSemverInfo = nextBetaVersion semverInfo
                normalize newSemverInfo

            let nextBuildVersionText (semverInfo: SemVerInfo) =
                let newSemverInfo = nextBuildVersion semverInfo
                normalize newSemverInfo

        [<RequireQualifiedAccess>]
        module ReleaseNotes =
            let private tbdHeaderLine (releaseNotes: ReleaseNotes.ReleaseNotes) =
                match releaseNotes.SemVer.PreRelease with 
                | Some prelease -> sprintf "## %s-%s - tbd" (SemVerInfo.mainVersionText releaseNotes.SemVer) prelease.Name
                | None -> 
                    let newPatchVersion = SemVerInfo.nextPatchVersion releaseNotes.SemVer
                    sprintf "## %s-alpha - tbd" newPatchVersion.AsString

            let private todayHeaderLine (releaseNotes: ReleaseNotes.ReleaseNotes) =
                sprintf "## %s - %s" releaseNotes.SemVer.AsString (DateTime.Now.ToString("yyyy-MM-dd"))        

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
                |> function
                    | fstReleaseNotes :: _ -> 
                        match fstReleaseNotes.Date with 
                        | None -> failwith "cannot find a tbd release note"
                        | Some _ -> fstReleaseNotes
                    | [] -> failwith "cannot find a tbd release note"

