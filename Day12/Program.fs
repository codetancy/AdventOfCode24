open System.Collections.Generic
open System.IO
open Common

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


module Offset =

    let ofDirection =
        function // (i, j)
        | North -> (-1, 0)
        | East -> (0, 1)
        | South -> (1, 0)
        | West -> (0, -1)

let (|InRegion|_|) garden label cell =
    match cell with
    | Patterns.InBounds garden (y, x) when garden[y, x] = label -> Some cell
    | _ -> None

let priceOf
    (cell: int * int)
    (garden: char[,])
    (label: char)
    (visited: HashSet<_>)
    =

    let mutable area = 0
    let mutable perimeter = 0

    let queue = Queue([ cell ])

    while queue.Count > 0 do

        let cell = queue.Dequeue()

        match cell with
        | InRegion garden label cell ->
            if not <| visited.Contains cell then
                visited.Add(cell) |> ignore

                do area <- Int.inc area

                Direction.Values
                |> Seq.map Offset.ofDirection
                |> Seq.map (fun (y, x) -> fst cell + y, snd cell + x)
                |> Seq.iter queue.Enqueue
        | _ -> perimeter <- Int.inc perimeter

    let result = area * perimeter
    // printfn $"{label} {area} * {perimeter} = {result}"
    result

let totalPriceOf (garden: char[,]) =

    let visited = HashSet()

    garden
    |> Array2D.toSeq
    |> Seq.filter (fun ((y, x), _) -> not <| visited.Contains(y, x))
    |> Seq.fold
        (fun acc (location, label) ->
            acc + priceOf location garden label visited)
        0

let garden = Parsing.parse "Datasets/Day12.txt"
let price = totalPriceOf garden

printfn $"{price}"
