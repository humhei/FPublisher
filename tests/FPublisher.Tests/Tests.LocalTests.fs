module Tests.LocalTests
#if INTERACTIVE
#load "../../.fake/build.fsx/intellisense_lazy.fsx"
#endif
open Expecto
open System.IO
open Fake.IO
open FPublisher
open FPublisher.Roles
open FPublisher.Nuget.Nuget


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


let localTests() =
  
  let role = NonGit.create Logger.Level.Normal (Some {ApiEnvironmentName = None; Serviceable = "http://127.0.0.1:4000/v3/index.json"; SearchQueryService = "http://127.0.0.1:4000/v3/search"}) localPackagesFolder workspace
  let localNugetServer: NugetServer =
    {ApiEnvironmentName = None; Serviceable = "http://127.0.0.1:4000/v3/index.json"; SearchQueryService = "http://127.0.0.1:4000/v3/search"}
  testList "NonGit tests" [
    testCase "push to local nuget" <| fun _ ->
    
      let paths =
          [
              //@"D:\VsCode\Workspace\Akkling" 
              //@"D:\VsCode\Workspace\FPublisher"
              //@"C:\Users\Jia\Desktop\hello"
              //@"D:\VsCode\Workspace\Shrimp.FileWatcher" 
              @"D:\VsCode\Github\FCSWatch"
              //@"D:\VsCode\Workspace\LiteDB"
              //@"D:\VsCode\Workspace\LiteDB.FSharp"
              //@"D:\VsCode\Workspace\Shrimp.Compiler.Service"
              //@"D:\VsCode\Workspace\Shrimp.FSharp.Plus"

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
              //@"D:\VsCode\Workspace\Shrimp.Workflow\src\Tasks\Shrimp.Workflow.Verifydocuments"
              //@"D:\VsCode\Workspace\Shrimp.Workflow\src\Tasks\Shrimp.Workflow.PressPrinting"
              //@"D:\VsCode\Workspace\Shrimp.Workflow\src\Tasks\Shrimp.Workflow.PressPrinting.TwoFaces"
              //@"D:\VsCode\Workspace\Shrimp.Workflow\src\Tasks\Shrimp.Workflow.DigitalPrinting"
              //@"d:\vscode\workspace\shrimp.workflow\tools\ws"             
              //@"D:\Users\Jia\Documents\MyData\Docs\2017\Packages"                 
              //@"D:\VsCode\Workspace\ExcelDnaWidget\QuickImpose"                 
              //@"D:\VsCode\Workspace\ExcelDnaWidget\QuickImpose.Book"                       
              //@"D:\VsCode\Workspace\ExcelDnaWidget.Core"   
              //@"D:\VsCode\Workspace\ExcelDnaWidget"   
          ]                                                                      




      try
          for root in paths do                                                                      
              let workspace = (Workspace root)
              let role = NonGit.create Logger.Level.Normal (Some localNugetServer) localPackagesFolder workspace
              //NonGit.run (NonGit.Target.Clean) role
              NonGit.run (NonGit.Target.PushToLocalNugetServerV3) role
              |> ignore                                                                  

      finally
        printfn "PressAnyKey to exists"

        System.Console.Read()
        |> ignore


  
  ]                                                   
                                           
             