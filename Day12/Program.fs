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

let priceOf
    (cell: int * int)
    (garden: char[,])
    (label: char)
    (visited: HashSet<_>)
    : int64 =
    let inline (+) (p1: int * int) (p2: int * int) =
        fst p1 + fst p2, snd p1 + snd p2

    let mutable area = 0
    let mutable perimeter = 0

    let queue = Queue([ cell ])

    while queue.Count > 0 do

        let cell = queue.Dequeue()

        match regionOf garden label cell with
        | Region.In ->
            if not <| visited.Contains cell then
                visited.Add(cell) |> ignore

                do area <- Int.inc area

                Direction.Values
                |> Seq.map (fun dir -> Offset.ofDirection dir + cell)
                |> Seq.iter queue.Enqueue
        | Region.Out -> perimeter <- Int.inc perimeter

    let result = area * perimeter
    // printfn $"{label} {area} * {perimeter} = {result}"
    result

let totalPriceOf (garden: char[,]) =

    let visited = HashSet()

    garden
    |> Array2D.toSeq
    |> Seq.filter (fun (coords, _) -> not <| visited.Contains(coords))
    |> Seq.fold
        (fun acc (coords, label) -> acc + priceOf coords garden label visited)
        0L

let garden = Parsing.parse "Datasets/Day12.txt"
let price = totalPriceOf garden

printfn $"{price}"
