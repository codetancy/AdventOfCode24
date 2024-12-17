open System
open System.IO
open Common
open Common.Patterns

module Parser =

    let parse filename =
        File.stream filename |> Seq.map (fun byte -> byte - int '0')

module Solution =

    let toLayout diskMap =
        diskMap
        |> Seq.indexed
        |> Seq.collect (fun (idx, memory) ->
            let id =
                match idx with
                | Even -> Some(idx / 2)
                | Odd -> None

            Seq.replicate memory id)

    let compact memory =

        let mutable i = 0
        let mutable j = Array.findIndexBack Option.isSome memory

        while i < j do

            match memory[i], memory[j] with
            | None, Some _ -> Array.swap i j memory
            | _, None -> j <- dec j
            | Some _, _ -> i <- inc i

        memory

    let checksum (layout: int option seq) =

        layout
        |> Seq.filter Option.isSome
        |> Seq.indexed
        |> (fun seq -> (0L, seq))
        ||> Seq.fold (fun acc (idx, maybeId) ->
            match maybeId with
            | Some id -> acc + int64 idx * int64 id
            | None -> acc)

    let solve diskMap =

        diskMap |> toLayout |> Array.ofSeq |> compact |> checksum

let input = Parser.parse "Datasets/Day09.txt"
let solution = Solution.solve input

printfn $"{solution}"
