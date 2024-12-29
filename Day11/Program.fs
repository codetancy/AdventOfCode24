open System.Collections.Generic
open System.Data
open System.IO

open Common
open Gomu.Option
open Microsoft.FSharp.Core

module Parsing =

    let parse filename =
        File.ReadLines filename
        |> Seq.map (fun line -> line.Split(' '))
        |> Seq.collect id
        |> Seq.map int64
        |> List.ofSeq

let (|First|_|) n =
    if n = LanguagePrimitives.GenericZero then
        Some LanguagePrimitives.GenericOne
    else
        None

let (|Second|_|) n =
    match Int.digits n with
    | Int.Even digits ->
        let divisor = pown 10L (digits / 2)
        Some(n / divisor, n % divisor)
    | Int.Odd _ -> None

let (|Third|_|) n = Some(2024L * n)

let blink =
    function
    | First next -> [ next ]
    | Second(left, right) -> [ left; right ]
    | Third next -> [ next ]
    | _ -> failwith "Unreachable"

let rec blinkStones remaining turn stones (cache: Dictionary<int * int64, _>) =

    (0L, stones)
    ||> Seq.fold (fun acc stone ->
        let maybeCached = cache.TryGetValue((turn, stone)) |> Option.fromTuple

        match maybeCached with
        | Some cached -> acc + cached
        | None ->
            if remaining = 0 then
                cache.Add((turn, stone), 1L)
                acc + 1L
            else
                let stones' = blink stone

                let result =
                    blinkStones
                        (Int.dec remaining)
                        (Int.inc turn)
                        stones'
                        cache

                cache.Add((turn, stone), result)

                acc + result)

let solve remaining stones =
    let cache = Dictionary()
    blinkStones remaining 0 stones cache

let input = Parsing.parse "Datasets/Day11.txt"

let part1 = solve 25 input
printfn $"{part1}"

let part2 = solve 75 input
printfn $"{part2}"
