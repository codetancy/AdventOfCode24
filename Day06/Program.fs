open System
open System.IO

open Common

module Parser =

    let parseLine line =
        line
        |> Array.ofSeq
        |> Array.map (function
            | '.' -> 0
            | '^' -> 1
            | '#' -> -1
            | c -> failwith $"Unrecognized character {c}")

    let parse lines = lines |> Seq.map parseLine |> array2D

module S1 =

    type Direction =
        | NORTH
        | EAST
        | SOUTH
        | WEST

    let offset =
        function
        | NORTH -> 0, -1 
        | EAST -> 1, 0
        | SOUTH -> 0, 1
        | WEST -> -1, 0

    let next =
        function
        | NORTH -> EAST
        | EAST -> SOUTH
        | SOUTH -> WEST
        | WEST -> NORTH

    let (|InBounds|_|) (floor: int[,]) (y, x) =
        if
            (y >= 0 && y < Array2D.length1 floor)
            && (x >= 0 && x < Array2D.length2 floor)
        then
            Some(y, x)
        else
            None

    let patrol floor =

        let rec loop (floor: int[,]) trail =
            let y0, x0, direction = List.head trail
            let x, y = offset direction

            match y0 + y, x0 + x with
            | InBounds floor (y1, x1) ->
                if floor[y1, x1] = -1 then
                    let trail = (y0, x0, next direction) :: List.tail trail
                    loop floor trail
                else
                    do Array2D.mapAt y1 x1 (fun n -> n + 1) floor
                    let trail = (y1, x1, direction) :: trail
                    loop floor trail
            | _ -> floor, trail
        
        let y0, x0 = Array2D.find 1 floor
        let trail = [y0, x0, Direction.NORTH]
        loop floor trail

let input = File.ReadAllLines "Files/Sample.txt"
let floor, trail = input |> Parser.parse |> S1.patrol

printfn $"{floor |> Array2D.count (fun n -> n > 0)}"