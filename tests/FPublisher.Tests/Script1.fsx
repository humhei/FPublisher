#r "nuget: FParsec"
open FParsec
open FParsec.CharParsers

let a = 1.0
let b = System.Int32.Parse (a.ToString())

let parser: Parser<_ ,unit> =
    //<PackageReference Include="System.ValueTuple" Version="4.5.0" />
    
    (anyString 100
    )

run parser """PackageReference Include="System.ValueTuple" Version="4.5.0" AASAAS SSSS    """
|> printfn "%A"
