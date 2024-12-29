open System.Collections.Generic
open System.IO
open Common

open Gomu.Arrays
open Gomu.Vectors
open Gomu.Tuples

module Parsing =

    let parse filename =
        File.ReadLines filename
        |> Seq.map (fun line -> line.ToCharArray())
        |> array2D

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
        function
        | North -> { X = -1; Y = 0 }
        | East -> { X = 0; Y = 1 }
        | South -> { X = 1; Y = 0 }
        | West -> { X = 0; Y = -1 }

[<RequireQualifiedAccess>]
type Region =
    | In
    | Out

let regionOf garden label =
    function
    | Array2D.InBounds garden p when garden[p.X, p.Y] = label -> Region.In
    | _ -> Region.Out

let countCorners (garden: char[,]) (label: char) cell =

    Direction.Diagonals
    |> Seq.map (fun (d1, d2) ->
        let d1 = Offset.ofDirection d1
        let d2 = Offset.ofDirection d2

        (d1, d2, d1 + d2)
        |> Triple.map (fun offset -> cell + offset)
        |> Triple.map (regionOf garden label))
    |> Seq.filter (function
        | Region.In, Region.In, Region.Out -> true // Inner
        | Region.Out, Region.Out, _ -> true //Outer
        | _ -> false)
    |> Seq.length

let priceOf cell (garden: char[,]) (label: char) (visited: HashSet<_>) =

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
                |> Seq.map (fun offset -> cell + offset)
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
