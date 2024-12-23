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

type Rule =
    | Single of int64
    | Double of int64 * int64

let (|Rule1|Rule2|Rule3|) (n: int64) =
    if n = 0L then
        Rule1(Single(n + 1L))
    else
        let nDigits = Int64.digits' n

        if Int.isEven nDigits then
            let divisor = pown 10L (nDigits / 2)
            Rule2(Double(n / divisor, n % divisor))
        else
            Rule3(Single(2024L * n))

let rec solve nBlinks (stones: int64 seq) =

    let blink stones =
        stones
        |> Seq.collect (fun stone ->
            match stone with
            | Rule1 n ->
                match n with
                | Single value -> [ value ]
                | _ -> failwith ""
            | Rule2 d ->
                match d with
                | Double(n1, n2) -> [ n1; n2 ]
                | _ -> failwith ""
            | Rule3 c ->
                match c with
                | Single value -> [ value ]
                | _ -> failwith "")

    if nBlinks = 0 then
        Seq.length stones
    else
        solve (nBlinks - 1) (blink stones)


let input = Parsing.parse "Datasets/Day11.txt"
printfn $"{Seq.print string input}"

let stones = solve 25 input
printfn $"{stones}"
