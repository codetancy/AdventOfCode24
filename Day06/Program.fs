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

type Tile = int * int
type Step = Tile * Direction

module S1 =

    let (|InBounds|_|) (floor: int[,]) (y, x) =
        if
            (y >= 0 && y < Array2D.length1 floor)
            && (x >= 0 && x < Array2D.length2 floor)
        then
            Some(y, x)
        else
            None

    let rec private loop (floor: int[,]) (visited: Set<Step>) (step: Step) =
        
        if Set.contains step visited then
            floor, visited, Outcome.Loop
        else
            let visited = Set.add step visited
            
            let (y0, x0), direction = step
            let x, y = offset direction

            match y0 + y, x0 + x with
            | InBounds floor (y1, x1) ->
                if floor[y1, x1] = -1 then
                    let visited = Set.remove step visited
                    let newStep = (y0, x0), next direction
                    loop floor visited newStep
                else
                    do Array2D.mapAt y1 x1 (fun n -> n + 1) floor
                    let nextStep = (y1, x1), direction
                    loop floor visited nextStep
            | _ -> floor, visited, Exited

    let patrol floor =
        let floor = Array2D.copy floor
        let y0, x0 = Array2D.find 1 floor
        let step = (y0, x0), Direction.NORTH
        loop floor Set.empty step

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
let floor', visited, outcome = input |> Parser.parse |> S1.patrol
let visitedCount = floor' |> Array2D.count (fun n -> n > 0)

printfn $"{visitedCount}"

// Part 2
let obstacles: Set<Tile> = visited |> Set.map fst

let loops =
    floor
    |> S2.paradox obstacles
    |> Seq.filter (fun outcome -> outcome = Outcome.Loop)
    |> Seq.length
    
printfn $"{loops}"
