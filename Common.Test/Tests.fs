module Tests

open System
open Xunit
open Common
open Faqt

[<Fact>]
let ``My test`` () =
    let number, rest = List.span Char.IsDigit ['1'; 'c']
    
    number.Should().SequenceEqual(['1']) |> ignore
    rest.Should().SequenceEqual(['c']) |> ignore
    
    