module Tests

open System
open Xunit
open Common
open Faqt

[<Fact>]
let ``Test Span`` () =
    let number, rest = List.span Char.IsDigit [ '1'; 'c' ]

    number.Should().SequenceEqual([ '1' ]) |> ignore
    rest.Should().SequenceEqual([ 'c' ]) |> ignore


[<Fact>]
let ``Test Starts With`` () =

    let result = List.startsWith [ '1'; '2'; '3' ] [ '1'; '2'; '3'; '4' ]
    result.Should().Equals(true) |> ignore

    let result = List.startsWith [ '1'; '2'; '3'; '4' ] [ '1'; '2'; '3' ]
    result.Should().Equals(false) |> ignore
    
    let result = List.startsWith [ '1'; '2' ] [ '1'; '3' ]
    result.Should().Equals(false) |> ignore
