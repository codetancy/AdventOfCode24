module Tests

open System
open Xunit
open Common
open Faqt
open Gomu.Arrays
open Gomu.Vectors

module List =

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

        let result = List.startsWith [] [ '1'; '3' ]
        result.Should().Equals(false) |> ignore

        let result = List.startsWith [] []
        result.Should().Equals(false) |> ignore

    [<Fact>]
    let ``Test After`` () =

        let result = List.after [ '1'; '2' ] [ '1'; '2'; '3' ]
        result.Should().SequenceEqual([ '3' ]) |> ignore

        let result = List.after [ '1'; '2' ] [ '1'; '2' ]
        result.Should().SequenceEqual([]) |> ignore

        let result = List.tryAfter [ '1'; '2' ] [ '1' ]
        result.Should().BeNone() |> ignore

        let result = List.tryAfter [ '1'; '2' ] [ '3'; '4' ]
        result.Should().BeNone() |> ignore

module Seq =

    [<Fact>]
    let ``Split Sequence`` () =
        let numbers =
            seq {
                for i in 1..10 do
                    yield if i % 3 = 0 then -1 else i
            }

        let splitted = Seq.split (fun i -> i < 0) numbers
        ()

module Array2D =

    [<Fact>]
    let ``Test toSeq`` () =

        let array = array2D [ [ 1; 2 ]; [ 3; 4 ] ]

        let expected =
            [ { X = 0; Y = 0 }, 1
              { X = 0; Y = 1 }, 2
              { X = 1; Y = 0 }, 3
              { X = 1; Y = 1 }, 4 ]

        let actual = Array2D.toSeq array
        actual.Should().SequenceEqual(expected)

    [<Fact>]
    let ``Test count`` () =

        let array = array2D [ [ 1; 2 ]; [ 3; 4 ] ]

        let actual = Array2D.count (fun n -> n % 2 = 0) array

        actual.Should().Be(2)

    [<Fact>]
    let ``Test find`` () =

        let array = array2D [ [ 1; 2 ]; [ 3; 4 ] ]

        let actual = Array2D.find 3 array

        actual.Should().Be({ X = 1; Y = 0 })

    let ``Test fold`` () =

        let array = array2D [ [ 1; 2 ]; [ 3; 4 ] ]

        let actual =
            Array2D.fold
                (fun acc (_, value) ->
                    match value % 2 = 0 with
                    | true -> acc + value
                    | _ -> acc)
                0
                array

        actual.Should().Be(6)
