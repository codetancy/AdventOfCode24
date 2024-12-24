open System.Data
open System.IO
open Common

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
    | Patterns.Even digits ->
        let divisor = pown 10L (digits / 2)
        Some(n / divisor, n % divisor)
    | Patterns.Odd _ -> None

let (|Third|_|) n = Some(2024L * n)

let rec solve nBlinks (stones: int64 seq) =

    let blink stones =
        stones
        |> Seq.collect (fun stone ->
            match stone with
            | First next -> [ next ]
            | Second(left, right) -> [ left; right ]
            | Third next -> [ next ]
            | _ -> failwith "Unreachable")

    if nBlinks = 0 then
        Seq.length stones
    else
        solve (nBlinks - 1) (blink stones)


let input = Parsing.parse "Datasets/Day11.txt"
printfn $"{Seq.print string input}"

let stones = solve 25 input
printfn $"{stones}"
