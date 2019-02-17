module Tests.MyTests
#if INTERACTIVE
#load "../.fake/build.fsx/intellisense_lazy.fsx"
#endif
open Expecto
open FPublisher
open System.IO
open Fake.IO
open System.Text.RegularExpressions

let pass() = Expect.isTrue true "passed"
let fail() = Expect.isTrue false "failed"


let MyTests =
  testList "faker tests" [

    testCase "test1" <| fun _ ->
      pass()

    testCase "test2" <| fun _ ->
      pass()

  ]