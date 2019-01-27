module Tests.MyTests
open Expecto
let pass() = Expect.isTrue true "passed"
let fail() = Expect.isTrue false "failed"
let MyTests =
  testList "MyTests" [
    testCase "MyTest" <| fun _ -> 
        pass()
  ]