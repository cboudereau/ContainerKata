[<AutoOpen>]
module Core
    let private pretty x = sprintf "%A" x
    let shouldBe expected actual =  
        if expected <> actual then raise (Xunit.Sdk.EqualException(pretty expected, pretty actual))

    module Result = 
        let get x = match x with Ok x' -> x' | e -> failwithf "expected a Ok result, got %A" e

    module Operators = 
        //https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/symbol-and-operator-reference/
        let (==) actual expected = shouldBe expected actual
