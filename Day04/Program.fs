open System
open System.IO

open Common
open Common.Patterns

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
    | North -> -1, 0
    | NorthEast -> -1, 1
    | East -> 0, 1
    | SouthEast -> 1, 1
    | South -> 1, 0
    | SouthWest -> 1, -1
    | West -> 0, -1
    | NorthWest -> -1, -1

module State =
    let next =
        function
        | 'X' -> 'M'
        | 'M' -> 'A'
        | 'A' -> 'S'
        | c -> failwith $"Unrecognized character {c}"

module S1 =
    let solve (wordSearch: char[,]) =

        let rec search (y0, x0) direction state =
            match state with
            | 'S' -> Some direction
            | _ ->
                let y, x = offset direction

                match y0 + y, x0 + x with
                | InBounds wordSearch (y1, x1) ->
                    match State.next state with
                    | nextState when nextState = wordSearch[y1, x1] ->
                        search (y1, x1) direction nextState
                    | _ -> None

                | outOfBounds -> None

        (0, wordSearch)
        ||> Array2D.fold (fun acc ((i, j), value) ->
            match value with
            | 'X' ->
                let solutions =
                    Direction.Values
                    |> List.choose (fun dir -> search (i, j) dir 'X')
                    |> List.length

                acc + solutions
            | _ -> acc)

module S2 =
    let solve (wordSearch: char[,]) =

        (0, wordSearch)
        ||> Array2D.fold (fun acc ((y0, x0), value) ->
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
                            let y, x = offset dir

                            match y0 + y, x0 + x with
                            | InBounds wordSearch (y1, x1) ->
                                Some wordSearch[y1, x1]
                            | outOfBounds -> None)
                        |> function
                            | [ Some 'M'; Some 'S' ]
                            | [ Some 'S'; Some 'M' ] -> 1
                            | _ -> 0)
                    |> List.forall (fun x -> x = 1)

                match xmas with
                | true -> acc + 1
                | false -> acc
            | _ -> acc)

let input = File.ReadLines "Files/Message.txt" |> Parser.parse

let xmas = S1.solve input
printfn $"{xmas}"

let x_mas = S2.solve input
printfn $"{x_mas}"
