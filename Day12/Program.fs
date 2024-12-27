open System.Collections.Generic
open System.IO
open Common

module Parsing =

    let parse filename =
        File.ReadLines filename
        |> Seq.map (fun line -> line.ToCharArray())
        |> array2D

[<RequireQualifiedAccess>]
type Direction =
    | North
    | East
    | South
    | West

    static member Values = [ North; East; South; West ]

    static member Diagonals =
        [ North, East; South, East; South, West; North, West ]

module Offset =

    let ofDirection =
        function // (i, j)
        | Direction.North -> (-1, 0)
        | Direction.East -> (0, 1)
        | Direction.South -> (1, 0)
        | Direction.West -> (0, -1)

[<RequireQualifiedAccess>]
type Region =
    | In
    | Out

let regionOf garden label =
    function
    | Array2D.InBounds garden (y, x) when garden[y, x] = label -> Region.In
    | _ -> Region.Out

let countCorners (garden: char[,]) (label: char) (cell: int * int) =
    let inline (+) (p1: int * int) (p2: int * int) =
        fst p1 + fst p2, snd p1 + snd p2

    let regionOf' = regionOf garden label

    Direction.Diagonals
    |> Seq.map (fun (d1, d2) -> Offset.ofDirection d1, Offset.ofDirection d2)
    |> Seq.map (fun (d1, d2) -> d1, d2, d1 + d2)
    |> Seq.map (fun (d1, d2, d3) -> d1 + cell, d2 + cell, d3 + cell)
    |> Seq.map (fun (n1, n2, n3) -> regionOf' n1, regionOf' n2, regionOf' n3)
    |> Seq.map (function
        | Region.In, Region.In, Region.Out -> 1 // Inner
        | Region.Out, Region.Out, _ -> 1 //Outer
        | _ -> 0)
    |> Seq.sum

let priceOf
    (cell: int * int)
    (garden: char[,])
    (label: char)
    (visited: HashSet<_>)
    =

    let mutable area = 0
    let mutable sides = 0
    let mutable perimeter = 0

    let queue = Queue([ cell ])

    while queue.Count > 0 do

        let cell = queue.Dequeue()

        match regionOf garden label cell with
        | Region.In ->
            if not <| visited.Contains cell then
                visited.Add(cell) |> ignore

                do area <- Int.inc area
                do sides <- sides + (countCorners garden label cell)

                Direction.Values
                |> Seq.map Offset.ofDirection
                |> Seq.map (fun offset ->
                    fst offset + fst cell, snd offset + snd cell)
                |> Seq.iter queue.Enqueue
        | Region.Out -> perimeter <- Int.inc perimeter

    area * perimeter, area * sides

let totalPriceOf (garden: char[,]) =

    let visited = HashSet()

    garden
    |> Array2D.toSeq
    |> Seq.filter (fun (coords, _) -> not <| visited.Contains(coords))
    |> Seq.fold
        (fun (totalRegular, totalBulk) (coords, label) ->
            let regular, bulk = priceOf coords garden label visited
            totalRegular + int64 regular, totalBulk + int64 bulk)
        (0L, 0L)


let garden = Parsing.parse "Datasets/Day12.txt"
let regular, bulk = totalPriceOf garden

printfn $"Regular: {regular}"
printfn $"Bulk: {bulk}"
