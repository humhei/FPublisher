namespace FPublisher.Solution

open FPublisher.FSharpPlus
open FPublisher.FakeHelper


#nowarn "0104"
open System.IO
open Fake.DotNet
open Fake.IO
open Fake.IO.Globbing.Operators
open Fake.IO.FileSystemOperators
open Fake.Core
open FPublisher.FakeHelper.CommandHelper
open FPublisher

#nowarn "0104"

[<AutoOpen>]
module _SolutionTypesExtensoins2 =  
    type DotNet.BuildConfiguration with 
        member x.ToConfiguration0() =
            match x with 
            | DotNet.BuildConfiguration.Custom v -> BuildConfiguration0.Custom v
            | DotNet.BuildConfiguration.Debug -> BuildConfiguration0.Debug
            | DotNet.BuildConfiguration.Release -> BuildConfiguration0.Release

    type Project with 
        member x.GetOutputPaths(configuration: DotNet.BuildConfiguration) =
            x.GetOutputPaths(configuration.ToConfiguration0())
                

        member x.GetOutputDirs (configuration: DotNet.BuildConfiguration) =
            x.GetOutputDirs(configuration.ToConfiguration0())


[<RequireQualifiedAccess>]
module Project =
    let exec (configuration: DotNet.BuildConfiguration) (args: seq<string>) (project: Project) =
        let configurationText = configuration.ToString()
        project.TargetFrameworks.AsList
        |> List.iter (fun framework ->
            let outputDll =
                project.Projdir </> "bin" </> configurationText </> TargetFramework.name framework </> project.Name + ".dll"
                |> Path.nomarlizeToUnixCompitiable

            let outputExe = outputDll |> Path.changeExtension ".exe"

            let outputDir = Path.getDirectory outputDll

            match framework, Environment.isUnix with
            | TargetFramework.CoreApp _, _ ->
                dotnet outputDll args outputDir

            | TargetFramework.NetStandard _,_ -> failwith "cannot exec class library"

            | TargetFramework.FullFramework _, false ->
                exec outputDll args outputDir
            | TargetFramework.FullFramework _, true ->
                match Mono.monoPath with
                | Some mono ->
                    exec mono ([outputDll; outputExe] @ List.ofSeq args) outputDir
                | None ->
                    failwith "cannot find mono"

            | TargetFramework.Net _, _ -> 
                dotnet outputDll args outputDir
            | TargetFramework.Net_Windows _, _ -> failwith "cannot exec Net_Windows library"
            
        )

    let addPackage package version (project: Project) = 
        dotnet "add" [project.ProjPath; "package"; package; "-v"; version] (project.Projdir)



[<RequireQualifiedAccess>]
module Workspace =

    /// include paket-files
    let allfsprojses (workspace: Workspace) =
        !! (workspace.WorkingDir </> "./**/*.fsproj")

    /// exclude paket-files
    let fsprojses (workspace: Workspace) =
        allfsprojses workspace
        -- (workspace.WorkingDir </> "./paket-files/**/*.fsproj")

    let cleanBinAndObj (workspace: Workspace) =
        allfsprojses workspace
        |> Seq.map Path.getDirectory
        |> Workspace.cleanBinAndObjForDirs

    let createSlnWith slnPath isForce (workspace: Workspace) =

        if isForce then File.delete slnPath
        if not <| File.exists slnPath then
            let projectPaths = (fsprojses workspace) |> List.ofSeq

            Solution.checkValidSlnPath slnPath

            let slnName = Path.GetFileNameWithoutExtension slnPath

            dotnet "new" ["sln" ;"--name"; slnName] workspace.WorkingDir
            projectPaths
            |> Seq.iter (fun proj ->
                printfn "%s" proj
                dotnet (sprintf "sln %s add" slnPath) [proj] workspace.WorkingDir
            )


    let createDefaultSln isForce (workspace: Workspace) =
        createSlnWith workspace.DefaultSlnPath isForce workspace

