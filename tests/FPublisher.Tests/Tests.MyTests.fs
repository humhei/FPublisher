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
open System.Text.RegularExpressions
open FParsec

open FSharp.Linq.RuntimeHelpers



let pass() = Expect.isTrue true "passed"
let fail() = Expect.isTrue false "failed"


let root =  Path.getFullName (Path.Combine (__SOURCE_DIRECTORY__,"../../"))




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


let localPackagesFolder = Some @"D:\Users\Jia\Documents\MyData\Tools\LocalPackages"



let nonGitTests() =
  
  let role = NonGit.create Logger.Level.Normal (Some {ApiEnvironmentName = None; Serviceable = "http://127.0.0.1:4000/v3/index.json"; SearchQueryService = "http://127.0.0.1:4000/v3/search"}) localPackagesFolder workspace
  let localNugetServer =
    {ApiEnvironmentName = None; Serviceable = "http://127.0.0.1:4000/v3/index.json"; SearchQueryService = "http://127.0.0.1:4000/v3/search"}
  testList "NonGit tests" [
    testCase "Project test" <| fun _ ->
        let proj = 
            Project.create @"D:\VsCode\Github\LiteDB.FSharp\LiteDB.FSharp\LiteDB.FSharp.fsproj"

        proj
        |> Project.updatePackages localNugetServer 

        ()

    testCase "solution tests" <| fun _ ->

                
        let package: PackageReference = 
            { Name = "Fake.Core.Process" 
              Version = SemVer.parse "5.17.0"
              Tag = PackageReferenceTag.Include
            }

   
        let m = 
            role.Solution.Projects 
            |> List.groupBy(fun project -> project.GetProjectKind())
            |> List.filter(fun (projectKind, _) ->
                match projectKind with 
                | ProjectKind.CoreCli | ProjectKind.Library | ProjectKind.AspNetCore -> true
                | _ -> false
            )

        pass()


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
      let paths =
          [
              //@"D:\VsCode\Github\Akkling" 
                //@"D:\VsCode\Github\FPublisher"
              //@"C:\Users\Jia\Desktop\hello"
              //@"D:\VsCode\Github\Shrimp.FileWatcher" 
              //@"D:\VsCode\Github\FCSWatch"
              //@"D:\VsCode\Github\LiteDB"
              //@"D:\VsCode\Github\LiteDB.FSharp"
              @"D:\VsCode\Github\Shrimp.FSharp.Plus"
              //@"D:\VsCode\Github\Shrimp.Akkling.Cluster.Intergraction" 
              //@"D:\VsCode\Github\Shrimp.Compiler.Service"
              //@"D:\VsCode\Github\Shrimp.Pdf" 
              //@"D:\VsCode\Github\CellScript" 
              //@"D:\VsCode\Github\Shrimp.Pdf.DataTable\"
              //@"D:\VsCode\Github\ExcelProcesser"
              //@"D:\VsCode\Github\Shrimp.LiteDB" 
              //@"D:\VsCode\Github\Shrimp.Bartender"
              //@"D:\VsCode\Github\Shrimp.UI"
              //@"D:\VsCode\Github\Shrimp.UI\Shrimp.Model"
              //@"D:\VsCode\Github\Shrimp.UI\Server"
              //@"D:\VsCode\Github\Shrimp.Workflow\src\TypeProvider"
              //@"D:\VsCode\Github\Shrimp.Workflow\src\Orders"
              //@"D:\VsCode\Github\Shrimp.Workflow\src\Tasks\Shrimp.Workflow.VerifyDocuments"
              //@"D:\VsCode\Github\Shrimp.Workflow\src\Tasks\Shrimp.Workflow.DigitalPrinting"
              //@"D:\VsCode\Github\Shrimp.UI\ServerScripting\TaskHandling"
              //@"D:\VsCode\Github\Shrimp.UI\ServerScripting\DSL"
          ]

      for root in paths do
          let workspace = (Workspace root)
          let role = NonGit.create Logger.Level.Normal (Some localNugetServer) localPackagesFolder workspace
          //NonGit.run (NonGit.Target.Clean) role
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
