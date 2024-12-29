open System.IO
open Common

open Gomu.Arrays
open Gomu.Vectors

module Parsing =

    let parse filename =
        let lines = File.ReadLines filename
        lines |> Seq.map (fun line -> line |> Seq.map Char.toInt) |> array2D

let solve (topographicMap: int[,]) =

    let reachedFrom trailHead =
        let mutable reached = List.empty

        let rec dfs p0 (visited: Set<Vector2i>) =
            let visited = Set.add p0 visited

            let isInBounds point =
                match point with
                | Array2D.InBounds topographicMap point -> Some point
                | _ -> None

            let hasNotBeenVisited point =
                if Set.contains point visited then None else Some point

            let isNext height point =
                if topographicMap[point.X, point.Y] = height + 1 then
                    Some point
                else
                    None

            match topographicMap[p0.X, p0.Y] with
            | 9 -> reached <- p0 :: reached
            | height ->
                Orthogonal.Values
                |> List.map Offset.ofOrthogonal
                |> List.map (fun d -> p0 + d)
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
