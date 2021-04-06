module Tests.MyTests
#if INTERACTIVE
#load "../../.fake/build.fsx/intellisense_lazy.fsx"
#endif
open Expecto
open System.IO
open Fake.IO
open FPublisher
open FPublisher.Roles
open Fake.Core
open FPublisher.Nuget
open FPublisher.Solution

open FSharp.Linq.RuntimeHelpers



let pass() = Expect.isTrue true "passed"
let fail() = Expect.isTrue false "failed"


//let root =  Path.getFullName (Path.Combine (__SOURCE_DIRECTORY__,"../../"))

//let root = @"D:\VsCode\Github\FCSWatch"
//let root = @"D:\VsCode\Github\ExcelProcesser"
//let root = @"D:\VsCode\Github\Shrimp.Pdf" 
//let root = @"D:\VsCode\Github\Shrimp.FSharp.Plus" 
//let root = @"D:\VsCode\Github\Shrimp.LiteDB" 
let root = @"D:\VsCode\Github\LiteDB.FSharp"
//let root = @"D:\VsCode\Github\CellScript" 
//let root = @"D:\VsCode\Github\Shrimp.UI"
//let root = @"D:\VsCode\Github\Shrimp.UI\Shrimp.Model"
//let root = @"D:\VsCode\Github\Shrimp.UI\Server"
//let root = @"D:\VsCode\Github\Shrimp.UI\ServerScripting\OrdersTable"
//let root = @"D:\VsCode\Github\Shrimp.UI\ServerScripting\TaskHandling"
//let root = @"D:\VsCode\Github\Shrimp.UI\ServerScripting\DSL"
//let root = @"D:\VsCode\Github\Shrimp.Pdf.DataTable\"
//let root = @"D:\VsCode\Github\Shrimp.Bartender"
//let root = @"D:\VsCode\Github\Shrimp.Compiler.Service"
//let root = @"D:\VsCode\Github\Shrimp.Akkling.Cluster.Intergraction"

#if !FAKE
let execContext = Fake.Core.Context.FakeExecutionContext.Create false "generate.fsx" []
Fake.Core.Context.setExecutionContext (Fake.Core.Context.RuntimeContext.Fake execContext)
#endif

let workspace = (Workspace root)

//let workspaceTests() =
//  testList "Workspace tests" [
//    testCase "add default solution" <| fun _ ->
//      Workspace.createDefaultSln false workspace

//    testCase "clean bin and obj" <| fun _ ->
//      Workspace.cleanBinAndObj workspace
//  ]






let nonGitTests() =
  let role = NonGit.create Logger.Level.Normal (Some {ApiEnvironmentName = None; Serviceable = "http://127.0.0.1:4000/v3/index.json"; SearchQueryService = "http://127.0.0.1:4000/v3/search"}) workspace
   
  testList "NonGit tests" [
    testCase "solution tests" <| fun _ ->
        let m = 
            role.Solution.Projects 
            |> List.groupBy(fun project -> project.GetProjectKind())
            |> List.filter(fun (projectKind, _) ->
                match projectKind with 
                | ProjectKind.CoreCli | ProjectKind.Library | ProjectKind.AspNetCore -> true
                | _ -> false
            )


        ()

    testCase "paket install" <| fun _ ->
      NonGit.run (NonGit.Target.InstallPaketPackages) role
      |> ignore

    testCase "build projects" <| fun _ ->
      NonGit.run (NonGit.Target.Build id) role
      |> ignore

    testCase "pack projects" <| fun _ ->
      NonGit.run (NonGit.Target.Pack id) role
      |> ignore

    ftestCase "push to local nuget" <| fun _ ->
      NonGit.run (NonGit.Target.PushToLocalNugetServerV3) role
      |> ignore

    //testCase "test projects" <| fun _ ->
    //  BuildServer.run (!^ NonGit.Msg.Test) role
  ]



//let forkerTests() =
//  testList "forker tests" [
//    testCase "pack pagckages" <| fun _ ->
//      let lastReleaseNotes = ReleaseNotes.loadLast role.Workspace.ReleaseNotesFile
//      BuildServer.run (!^ (Forker.Msg.Pack lastReleaseNotes.Value)) role

//    ftestCase "publish to local nuget server" <| fun _ ->
//      BuildServer.run (!^ (Forker.Msg.PublishToLocalNugetServer)) role
//  ]

//let collaboratorTests() =
//  testList "Collaborator Tests" [
//    testCase "next release" <| fun _ ->
//      BuildServer.run (!^ Collaborator.Msg.NextRelease) role
//  ]

//let buildServerTests() =
//  testList "BuildServer Tests" [
//    testCase "RunCI" <| fun _ ->
//      BuildServer.run (BuildServer.Msg.RunCI) role
//  ]
