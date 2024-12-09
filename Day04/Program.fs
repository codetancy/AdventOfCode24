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
    let solve (grid: char[,]) =

        // fugly
        let rec search (y0, x0) direction state =
            if state = 'S' then
                Some direction
            else
                let y, x = offset direction

                match y0 + y, x0 + x with
                | InBounds grid (y1, x1) ->
                    let nextState = State.next state
                    let actualLetter = grid[y1, x1]

                    if nextState = actualLetter then
                        search (y1, x1) direction nextState
                    else
                        None
                | _ -> None

        (0, grid)
        ||> Array2D.foldi (fun acc i j value ->
            match value with
            | 'X' ->
                let solutions =
                    Direction.Values
                    |> List.choose (fun dir -> search (i, j) dir 'X')
                    |> List.length

                acc + solutions
            | _ -> acc)

let input = File.ReadLines "Files/Message.txt" |> Parser.parse

let solution = S1.solve input

printfn $"{solution}"
