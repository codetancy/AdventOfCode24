open System
open System.IO

open Common
open Common.Patterns

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

    let patrol (floor: int[,]) =

        let rec loop (visited: Set<Step>) (step: Step) =
            if Set.contains step visited then
                visited, Outcome.Loop
            else
                let visited = Set.add step visited

                let (y0, x0), direction = step
                let x, y = offset direction

                match y0 + y, x0 + x with
                | InBounds floor (y1, x1) ->
                    if floor[y1, x1] = -1 then
                        let visited = Set.remove step visited
                        let newStep = (y0, x0), next direction
                        loop visited newStep
                    else
                        let nextStep = (y1, x1), direction
                        loop visited nextStep
                | _ -> visited, Exited

        let y0, x0 = Array2D.find 1 floor
        let step = (y0, x0), Direction.NORTH
        loop Set.empty step

module S2 =
    open S1

    let paradox obstacles floor =
        let y0, x0 = Array2D.find 1 floor
        let obstacles = Set.remove (y0, x0) obstacles
        let count = Set.count obstacles

        obstacles
        |> Seq.indexed
        |> Seq.map (fun (i, (y, x)) ->
            // do printf $"{i}/{count} ="
            do floor[y, x] <- -1
            let _, outcome = patrol floor
            do floor[y, x] <- 0
            // do printfn $"{outcome}"
            outcome)

let input = File.ReadAllLines "Datasets/Day06.txt"
let floor = input |> Parser.parse

// Part 1
let visited, outcome = floor |> S1.patrol
let visitedTiles = visited |> Set.map fst

printfn $"{Set.count visitedTiles}"

// Part 2
let loops =
    floor
    |> S2.paradox visitedTiles
    |> Seq.filter (fun outcome -> outcome = Outcome.Loop)
    |> Seq.length

printfn $"{loops}"
