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

    type Outcome =
        | Exited
        | Loop

    let (|InBounds|_|) (floor: int[,]) (y, x) =
        if
            (y >= 0 && y < Array2D.length1 floor)
            && (x >= 0 && x < Array2D.length2 floor)
        then
            Some(y, x)
        else
            None

    let rec private loop (floor: int[,]) trail =
        let head = List.head trail
        let tail = List.tail trail

        let y0, x0, direction = head

        if List.contains (y0, x0, direction) tail then
            floor, List.rev trail, Outcome.Loop
        else
            let x, y = offset direction

            match y0 + y, x0 + x with
            | InBounds floor (y1, x1) ->
                if floor[y1, x1] = -1 then
                    let trail = (y0, x0, next direction) :: tail
                    loop floor trail
                else
                    do Array2D.mapAt y1 x1 (fun n -> n + 1) floor
                    let trail = (y1, x1, direction) :: trail
                    loop floor trail
            | _ -> floor, List.rev trail, Exited

    let patrol floor =
        let floor = Array2D.copy floor
        let y0, x0 = Array2D.find 1 floor
        let trail = [ y0, x0, Direction.NORTH ]
        loop floor trail

module S2 =
    open S1

    let paradox obstacles floor =
        let y0, x0 = Array2D.find 1 floor
        let obstacles = Set.remove (y0, x0) obstacles
        let count = Set.count obstacles

        obstacles
        |> Seq.indexed
        |> Seq.map (fun (i, (y, x)) ->
            let floor = Array2D.copy floor
            do floor[y, x] <- -1
            let _, _, outcome = patrol floor
            outcome)

let input = File.ReadAllLines "Files/Floor.txt"
let floor = input |> Parser.parse

// Part 1
let floor', trail, outcome = input |> Parser.parse |> S1.patrol
let visited = floor' |> Array2D.count (fun n -> n > 0)

printfn $"{visited}"

// Part 2
let obstacles = trail |> List.map (fun (y, x, _) -> y, x) |> Set.ofList
let loops =
    floor
    |> S2.paradox obstacles
    |> Seq.filter (fun outcome -> outcome = S1.Outcome.Loop)
    |> Seq.length
    
printfn $"{loops}"
