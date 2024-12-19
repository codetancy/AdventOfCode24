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


let countTrails (topographicMap: int[,]) =

    let scoreOf i j height =

        let mutable reached = Set.empty

        let rec dfs y0 x0 height (visited: Set<int * int>) =
            // do printfn $"{y0},{x0}"
            let visited = Set.add (y0, x0) visited

            let isInBounds (y, x) =
                match y, x with
                | Patterns.InBounds topographicMap _ -> true
                | _ -> false

            let wasVisited (y, x) = Set.contains (y, x) visited

            let isNext (y, x) = height + 1 = topographicMap[y, x]

            match height with
            | 9 ->
                // do printfn "Adding one"
                reached <- Set.add (y0, x0) reached
            | _ ->
                Direction.Values
                |> List.map Direction.toOffset
                |> List.map (fun (y, x) -> y0 + y, x + x0)
                |> List.filter isInBounds
                |> List.filter (not << wasVisited)
                |> List.filter isNext
                |> List.iter (fun (y1, x1) -> dfs y1 x1 (height + 1) visited)

        if height = 0 then
            // do printfn $"START {i} {j}"
            dfs i j height Set.empty

        Set.count reached

    topographicMap |> Array2D.mapi scoreOf

let topograficMap = Parsing.parse "Datasets/Day10.txt"
let trails = countTrails topograficMap

let solution = Array2D.toSeq trails |> Seq.map snd |> Seq.sum
printfn $"{solution}"
