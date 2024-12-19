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

    let compact memory =

        let (|File|Free|) n = if n < 0 then Free else File

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

    let toMemoryMap layout =

        Seq.indexed layout
        |> Seq.groupBy snd
        |> Seq.map (fun (key, els) ->
            let indices = Seq.map fst els |> Array.ofSeq
            let start = Array.head indices
            let length = Array.last indices - start + 1
            key, Memory(layout, start, length))
        |> Array.ofSeq

    let compact' (memory: (int * Memory<int>)[]) =

        let files =
            memory
            |> Seq.indexed
            |> Seq.filter (fun (i, (id, block)) -> id >= 0)
            |> Seq.rev

        let spaces =
            memory |> Seq.indexed |> Seq.filter (fun (i, (id, block)) -> id < 0)

        for fileIndex, file in files do

            let fileId, fileBlock = file

            let maybeFreeSpaceIndex =
                spaces
                |> Seq.tryFind (fun (_, (_, freeBlock)) ->
                    freeBlock.Length >= fileBlock.Length)

            match maybeFreeSpaceIndex with
            | Some(freeSpaceIndex, free) ->

                if freeSpaceIndex < fileIndex then
                    let freeId, freeBlock = free

                    fileBlock
                    |> Memory.swap (freeBlock.Slice(0, fileBlock.Length))

                    let updatedFreeBlock = freeBlock.Slice(fileBlock.Length)
                    memory[freeSpaceIndex] <- freeId, updatedFreeBlock

            | None -> ()

    let solve diskMap =
        toLayout diskMap |> Array.ofSeq |> compact |> checksum

    let solve' diskMap =
        let layout = Array.ofSeq <| toLayout diskMap
        let memory = toMemoryMap layout
        compact' memory
        checksum layout

let input = Parser.parse "Datasets/Day09.txt" |> Seq.cache
let solution = Solution.solve input

printfn $"{solution}"

let solution2 = Solution.solve' input

printfn $"{solution2}"
