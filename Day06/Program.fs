open System.IO

open Common
open Gomu.Vectors
open Gomu.Arrays

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

module Direction =

    let next =
        function
        | North -> East
        | East -> South
        | South -> West
        | West -> North

type Outcome =
    | Exited
    | Loop

type Step = Vector2i * Orthogonal

module S1 =

    let patrol (floor: int[,]) =

        let rec loop (visited: Set<Step>) (step: Step) =
            if Set.contains step visited then
                visited, Outcome.Loop
            else
                let visited = Set.add step visited

                let p0, direction = step
                let d = Offset.ofOrthogonal direction

                match p0 + d with
                | Array2D.InBounds floor p1 ->
                    if floor[p1.X, p1.Y] = -1 then
                        let visited = Set.remove step visited
                        let newStep = p0, Direction.next direction
                        loop visited newStep
                    else
                        let nextStep = p1, direction
                        loop visited nextStep
                | _ -> visited, Exited

        let p0 = Array2D.find 1 floor
        let step = p0, Orthogonal.North
        loop Set.empty step

module S2 =
    open S1

    let paradox obstacles floor =
        let startingPoint = Array2D.find 1 floor
        let obstacles = Set.remove startingPoint obstacles

        obstacles
        |> Seq.indexed
        |> Seq.map (fun (_, p) ->
            do floor[p.X, p.Y] <- -1
            let _, outcome = patrol floor
            do floor[p.X, p.Y] <- 0
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
