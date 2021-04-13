#r "nuget: FParsec"
open FParsec
open FParsec.CharParsers

let parser: Parser<_ ,unit> =
    //<PackageReference Include="System.ValueTuple" Version="4.5.0" />
    
    (pstringCI "PackageReference" 
        >>. spaces1 
        >>. (pstringCI "Update" <|> pstringCI "Include")
        >>. spaces 
        >>. pchar '='
        >>. spaces
        >>. between (pchar '"') (pchar '"') (many1Chars anyChar)
        .>>. spaces1
        >>. pstringCI "Version"
        >>. spaces 
        >>. pchar '='
        >>. spaces
        >>. between (pchar '"') (pchar '"') (many1Chars anyChar)
    )

run parser """PackageReference Include="System.ValueTuple" Version="4.5.0" """
