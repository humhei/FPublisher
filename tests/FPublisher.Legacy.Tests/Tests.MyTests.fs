module Tests.MyTests
#if INTERACTIVE
#load "../.fake/build.fsx/intellisense_lazy.fsx"
#endif
open Expecto
open FPublisher
open System.IO
open Fake.IO
open FPublisher
open System.Text.RegularExpressions
open FSharp.Formatting

let pass() = Expect.isTrue true "passed"
let fail() = Expect.isTrue false "failed"

let root =  Path.getFullName (Path.Combine (__SOURCE_DIRECTORY__,"../"))


let MyTests =
  testList "Interation tests" [
    testCase "generate doc" <| fun _ -> 
        
        pass()
  ]