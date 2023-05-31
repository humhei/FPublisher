module Tests.MyTests
#if INTERACTIVE
#load "../../.fake/build.fsx/intellisense_lazy.fsx"
#endif
open Expecto
open System.IO
open Fake.IO
open Fake.IO.Globbing.Operators
open FPublisher
open FPublisher.Roles
open Fake.Core
open FPublisher.Nuget
open FPublisher.Solution
open System.Text.RegularExpressions
open FParsec
open Shrimp.FSharp.Plus
open Deedle
open FSharp.Linq.RuntimeHelpers

let a =
    [1; 2; 3]
    |> List.minBy(fun m -> m)


let pass() = Expect.isTrue true "passed"
let fail() = Expect.isTrue false "failed"


let root =  Path.getFullName (Path.Combine (__SOURCE_DIRECTORY__,"../../"))

type A =
    { A: int 
      B: bool }


type B =
    { A: int 
      B: string }

let m = {A = 6; B = ""}
let cL: A = {A = 6; B = true }



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

type Hello =
    { Name: string 
      Age: int 
      Sex: string }
with 
    member x.Addr  = "MyAddreass"

let nonGitTests() =
  
  let role = NonGit.create Logger.Level.Normal (Some {ApiEnvironmentName = None; Serviceable = "http://127.0.0.1:4000/v3/index.json"; SearchQueryService = "http://127.0.0.1:4000/v3/search"}) localPackagesFolder workspace
  let localNugetServer: NugetServer =
    {ApiEnvironmentName = None; Serviceable = "http://127.0.0.1:4000/v3/index.json"; SearchQueryService = "http://127.0.0.1:4000/v3/search"}
  testList "NonGit tests" [
    testCase "Project test" <| fun _ ->
        let proj = 
            Project.create @"D:\VsCode\Workspace\LiteDB.FSharp\LiteDB.FSharp\LiteDB.FSharp.fsproj"

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
      //let clearTmpDir() =   
      //  let dir = Path.GetTempPath()
      //  let dirs = 
      //      Directory.EnumerateDirectories(dir)
      //      |> Seq.iter(fun dir ->
      //          Directory.delete dir
      //      )


      //  !! (dir + "\*.*")
      //  |> Seq.iter(fun file ->
      //      try File.delete file 
      //      with ex -> ()
      //  )

      //clearTmpDir()

      let paths =
          [
              //@"D:\VsCode\Workspace\Akkling" 
              //@"D:\VsCode\Workspace\FPublisher"
              //@"C:\Users\Jia\Desktop\hello"
              //@"D:\VsCode\Workspace\Shrimp.FileWatcher" 
              //@"D:\VsCode\Workspace\FCSWatch"
              //@"D:\VsCode\Workspace\LiteDB"
              //@"D:\VsCode\Workspace\LiteDB.FSharp"
              //@"D:\VsCode\Workspace\Shrimp.Compiler.Service"
              @"D:\VsCode\Workspace\Shrimp.FSharp.Plus"
              //@"D:\VsCode\Workspace\Shrimp.Akkling.Cluster.Intergraction" 
              //@"D:\VsCode\Workspace\CellScript" 


              //@"D:\VsCode\Workspace\Shrimp.LiteDB" 
              //@"D:\VsCode\Workspace\Shrimp.XllProxy" 
              //@"D:\VsCode\Workspace\Shrimp.Pdf" 
              //@"D:\VsCode\Workspace\Shrimp.Pdf.Enhancement" 
              //@"D:\VsCode\Workspace\Shrimp.Pdf.DataTable"
              //@"D:\VsCode\Workspace\ExcelProcesser"


              ///-------------------------------------------------------------
              /// Shrimp.LiteDB: LOCAL Background Application packages update
              ///-------------------------------------------------------------

              //@"D:\VsCode\Workspace\Shrimp.Acrobat"

              ///-------------------------------------------------------------
              /// Shrimp.Acrobat: LOCAL Background Application packages update
              ///-------------------------------------------------------------


              //@"D:\VsCode\Workspace\Shrimp.Bartender"

              ///-------------------------------------------------------------
              /// BARTENDER: LOCAL Background Application packages update
              ///-------------------------------------------------------------


              //@"D:\VsCode\Workspace\Shrimp.Workflow\src\Query"
              //@"D:\VsCode\Workspace\Shrimp.Workflow\src\Shrimp.Workflow.PdfPageInfos"



              //@"D:\VsCode\Workspace\Shrimp.Workflow\src\TypeProvider"

              ///-------------------------------------------------------------
              /// TypeProviderServer: LOCAL Background Application packages update
              ///-------------------------------------------------------------
                                                                      
                      
              //@"D:\VsCode\workspace\Shrimp.Workflow\src\CustomerSupply"  
              //@"D:\VsCode\Workspace\Shrimp.Workflow\src\Model"                   
              //@"D:\VsCode\Workspace\Shrimp.Workflow\src\Orders"                 
              //@"D:\VsCode\Workspace\Shrimp.Workflow\src\Shrimp.Workflow.Products.Integrated"   
              //@"D:\VsCode\Workspace\Shrimp.Workflow\src\Shrimp.Workflow.Products.AllProducts"   
              //@"D:\VsCode\Workspace\Shrimp.Workflow\src\Shrimp.Workflow.Products.TasksTarget"            
              //@"D:\VsCode\Workspace\Shrimp.Workflow\src\SuccessfulPrint.Integrated.Concated"                                                            
              //@"D:\VsCode\Workspace\Shrimp.Workflow\src\SuccessfulPrint.Plugin"                                                            
              //@"D:\VsCode\Workspace\Shrimp.Workflow\src\Tasks\Shrimp.Workflow.Orderform"                 
              //@"D:\VsCode\Workspace\Shrimp.Workflow\src\Tasks\Shrimp.Workflow.VerifyDocuments"
              //@"D:\VsCode\Workspace\Shrimp.Workflow\src\Tasks\Shrimp.Workflow.DigitalPrinting"
              //@"D:\VsCode\Workspace\Shrimp.Workflow\src\Tasks\Shrimp.Workflow.PressPrinting"
              //@"D:\VsCode\Workspace\Shrimp.Workflow\src\Tasks\Shrimp.Workflow.PressPrinting.TwoFaces"
              //@"d:\vscode\workspace\shrimp.workflow\tools\ws"             
              //@"D:\VsCode\Workspace\ExcelDnaWidget\QuickImpose"                
              //@"D:\VsCode\Workspace\ExcelDnaWidget\QuickImpose.Book"                       
              //@"D:\VsCode\Workspace\ExcelDnaWidget.Core"   
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
                             
                              
                                                        








