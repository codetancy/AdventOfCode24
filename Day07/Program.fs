open System

open System.IO
open Common

module Parser =

    let (|Number|_|) (chars: char list) =
        match List.span Char.IsDigit chars with
        | [], _ -> None
        | digits, rest ->
            Some(digits |> Array.ofList |> String |> Int64.Parse, rest)

    let parseLine (line: string) =

        let rec loop chars numbers =
            match chars with
            | [] -> List.rev numbers
            | Number(n, rest) -> loop rest (n :: numbers)
            | _ :: tail -> loop tail numbers

        loop (line.ToCharArray() |> List.ofArray) []

    let parse (lines: string array) = lines |> Array.map parseLine

type Operator =
    | Add
    | Multiply
    | Concat

type Operation = Operator * int64

module Operator =

    let func =
        function
        | Add -> (+)
        | Multiply -> (*)
        | Concat ->
            (fun x y -> int64 (pown 10.0 (Int64.digits y)) * x + y)

module S1 =

    let verify operators numbers =

        let target = List.head numbers

        let rec solve acc (operations: Operation list) =
            match operations with
            | [] -> acc
            | (op, value) :: rest ->
                let acc = (Operator.func op) acc value
                if acc > target then 0L else solve acc rest

        let acc, operands =
            match List.tail numbers with
            | acc :: operands -> acc, operands
            | [] -> failwith "List should not be empty"

        let operators = List.permutations' (List.length operands) operators

        let solutions =
            operators
            |> List.map (fun ops ->
                let operations = List.zip ops operands
                solve acc operations)
            |> List.filter (fun total -> total = target)

        match solutions with
        | [] -> 0L
        | _ -> target

let input = File.ReadAllLines "Files/Equations.txt" |> Parser.parse

let s1 =
    input
    |> Array.map (S1.verify [ Operator.Add; Operator.Multiply ])
    |> Array.sum

printfn $"{s1}"

let s2 =
    input
    |> Array.map (
        S1.verify [ Operator.Add; Operator.Multiply; Operator.Concat ]
    )
    |> Array.sum

printfn $"{s2}"
