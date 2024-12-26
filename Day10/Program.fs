open System
open System.IO
open Common

module Parsing =

    let parse filename =
        let lines = File.ReadLines filename
        lines |> Seq.map (fun line -> line |> Seq.map Char.toInt) |> array2D

type Direction =
    | North
    | East
    | South
    | West

    static member Values = [ North; East; South; West ]

module Direction =

    let toOffset =
        function
        | North -> (-1, 0)
        | East -> (0, 1)
        | South -> (1, 0)
        | West -> (0, -1)

let solve (topographicMap: int[,]) =

    let reachedFrom (trailHead: int * int) =
        let mutable reached = List.empty

        let rec dfs (y0, x0) (visited: Set<int * int>) =
            let visited = Set.add (y0, x0) visited

            let isInBounds (y, x) =
                match y, x with
                | Array2D.InBounds topographicMap _ -> Some(y, x)
                | _ -> None

            let hasNotBeenVisited (y, x) =
                if Set.contains (y, x) visited then None else Some(y, x)

            let isNext height (y, x) =
                if topographicMap[y, x] = height + 1 then
                    Some(y, x)
                else
                    None

            match topographicMap[y0, x0] with
            | 9 -> reached <- (y0, x0) :: reached
            | height ->
                Direction.Values
                |> List.map Direction.toOffset
                |> List.map (fun (y, x) -> y0 + y, x + x0)
                |> List.choose (fun potentialLocation ->
                    Some potentialLocation
                    |> Option.bind isInBounds
                    |> Option.bind hasNotBeenVisited
                    |> Option.bind (isNext height))
                |> List.iter (fun nextLocation -> dfs nextLocation visited)

        dfs trailHead Set.empty
        reached

    let scoreOf trails = Set.ofList trails |> Set.count
    let ratingOf trails = List.length trails

    let trailHeads =
        topographicMap
        |> Array2D.toSeq
        |> Seq.filter (fun (_, height) -> height = 0)
        |> Seq.map fst

    trailHeads
    |> Seq.fold
        (fun acc trailHead ->
            reachedFrom trailHead
            |> (fun trails -> scoreOf trails, ratingOf trails)
            |> (fun (score, rating) -> fst acc + score, snd acc + rating))
        (0, 0)

let topograficMap = Parsing.parse "Datasets/Day10.txt"
let score, rating = solve topograficMap

printfn $"{score},{rating}"
