﻿[<AutoOpen>]
module Core
    let private pretty x = sprintf "%A" x
    let shouldBe expected actual =  
        if expected <> actual then raise (Xunit.Sdk.EqualException(pretty expected, pretty actual))

    module Operators = 
        //https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/symbol-and-operator-reference/
        let (==) actual expected = shouldBe expected actual
