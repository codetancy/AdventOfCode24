open System.IO

open Gomu.Vectors
open Gomu.Arrays

module Parser =

    let parseLine (line: string) = line.ToCharArray()

    let parse lines = lines |> Seq.map parseLine |> array2D

type Direction =
    | North
    | NorthEast
    | East
    | SouthEast
    | South
    | SouthWest
    | West
    | NorthWest

    static member Values =
        [ North; NorthEast; East; SouthEast; South; SouthWest; West; NorthWest ]

let offset =
    function
    | North -> { X = -1; Y = 0 }
    | NorthEast -> { X = -1; Y = 1 }
    | East -> { X = 0; Y = 1 }
    | SouthEast -> { X = 1; Y = 1 }
    | South -> { X = 1; Y = 0 }
    | SouthWest -> { X = 1; Y = -1 }
    | West -> { X = 0; Y = -1 }
    | NorthWest -> { X = -1; Y = -1 }

module State =
    let next =
        function
        | 'X' -> 'M'
        | 'M' -> 'A'
        | 'A' -> 'S'
        | c -> failwith $"Unrecognized character {c}"

module S1 =
    let solve (wordSearch: char[,]) =

        let rec search p0 direction state =
            match state with
            | 'S' -> Some direction
            | _ ->
                let d = offset direction

                match p0 + d with
                | Array2D.InBounds wordSearch p1 ->
                    match State.next state with
                    | nextState when nextState = wordSearch[p1.X, p1.Y] ->
                        search p1 direction nextState
                    | _ -> None

                | outOfBounds -> None

        (0, wordSearch)
        ||> Array2D.fold (fun acc (p, value) ->
            match value with
            | 'X' ->
                let solutions =
                    Direction.Values
                    |> List.choose (fun dir -> search p dir 'X')
                    |> List.length

                acc + solutions
            | _ -> acc)

module S2 =
    let solve (wordSearch: char[,]) =

        (0, wordSearch)
        ||> Array2D.fold (fun acc (p0, value) ->
            match value with
            | 'A' ->
                let diagonals =
                    [ [ Direction.NorthWest; Direction.SouthEast ]
                      [ Direction.SouthWest; Direction.NorthEast ] ]

                let xmas =
                    diagonals
                    |> List.map (fun diagonal ->
                        diagonal
                        |> List.map (fun dir ->
                            let d = offset dir

                            match p0 + d with
                            | Array2D.InBounds wordSearch p1 ->
                                Some wordSearch[p1.X, p1.Y]
                            | _ -> None)
                        |> function
                            | [ Some 'M'; Some 'S' ]
                            | [ Some 'S'; Some 'M' ] -> 1
                            | _ -> 0)
                    |> List.forall (fun x -> x = 1)

                match xmas with
                | true -> acc + 1
                | false -> acc
            | _ -> acc)

let input = File.ReadLines "Datasets/Day04.txt" |> Parser.parse

let xmas = S1.solve input
printfn $"{xmas}"

let x_mas = S2.solve input
printfn $"{x_mas}"
