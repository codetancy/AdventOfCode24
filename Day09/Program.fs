open System
open System.IO
open Common
open Common.Int.Patterns
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
                | Even -> idx / 2
                | Odd -> -((idx / 2) + 1)

            Seq.replicate memory id)

    let (|File|Free|) n = if n < 0 then Free else File

    let compact memory =

        let mutable i = 0
        let mutable j = Array.findIndexBack (fun n -> n > 0) memory

        while i < j do

            match memory[i], memory[j] with
            | Free, File -> Array.swap i j memory
            | _, Free -> j <- dec j
            | File, _ -> i <- inc i

        memory

    let checksum (layout: int seq) =

        layout
        |> Seq.indexed
        |> (fun seq -> (0L, seq))
        ||> Seq.fold (fun acc (idx, id) ->
            match id with
            | Positive -> acc + int64 idx * int64 id
            | _ -> acc)

    let solve diskMap =
        toLayout diskMap |> Array.ofSeq |> compact |> checksum

let input = Parser.parse "Datasets/Day09.txt"
let solution = Solution.solve input

printfn $"{solution}"
