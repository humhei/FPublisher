// Learn more about F# at http://fsharp.org
module FPublisher.Fake.BuildOutputInPackage
open FPublisher.Faker.Helper
open System

let s a =
    let a = Class1()
    printfn "Hello World from F#!"
    0 // return an integer exit code
