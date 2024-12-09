open System

open System.IO
open System.Net
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

type Operation = Operator * int64

module Operator =

    let func =
        function
        | Add -> (+)
        | Multiply -> (*)

module S1 =

    let verify numbers =

        let rec solve acc (operations: Operation list) =
            match operations with
            | [] -> acc
            | (op, value) :: rest ->
                let acc = (Operator.func op) acc value
                solve acc rest

        let target = List.head numbers

        let acc, operands =
            match List.tail numbers with
            | acc :: operands -> acc, operands
            | [] -> failwith "List should not be empty"

        let operators =
            List.permutations'
                (List.length operands)
                [ Operator.Add; Operator.Multiply ]

        let solutions =
            operators
            |> List.map (fun ops ->
                let operations = List.zip ops operands
                solve acc operations)
            |> List.filter (fun total -> total = target)

        match solutions with
        | [] -> 0L
        | _ -> target

let some = File.ReadAllLines "Files/Equations.txt" |> Parser.parse
let sum = some |> Array.map S1.verify |> Array.sum

printfn $"{sum}"
