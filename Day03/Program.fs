module Day03

open System
open System.IO

open Common

type Keyword =
    | MUL
    | DO
    | DONT

type Token =
    | IDENTIFIER of Keyword
    | NUMBER of int
    | COMMA
    | LEFT_PAREN
    | RIGHT_PAREN
    | UNKNOWN

module Parser =

    let parse (lines: string seq) =
        lines |> Seq.map (fun s -> s.ToCharArray()) |> Seq.concat |> List.ofSeq

module S1 =

    module Patterns =
        let (|Number|_|) (ls: char list) =
            match List.span Char.IsDigit ls with
            | [], _ -> None
            | digits, tail ->
                let number = digits |> Array.ofList |> String |> int
                Some(number, tail)

        let (|Keyword|_|) (keyword: string) (ls: char list) =
            let keyword = keyword.ToCharArray() |> List.ofArray
            List.tryAfter keyword ls

    open Patterns
    
    let tokenize input =

        let rec loop input tokens =
            match input with
            | [] -> List.rev tokens
            | Number(n, rest) -> loop rest (NUMBER(n) :: tokens)
            | Keyword "mul" rest -> loop rest (IDENTIFIER(MUL) :: tokens)
            | Keyword "don't" rest -> loop rest (IDENTIFIER(DONT) :: tokens)
            | Keyword "do" rest -> loop rest (IDENTIFIER(DO) :: tokens)
            | '(' :: xs -> loop xs (LEFT_PAREN :: tokens)
            | ',' :: xs -> loop xs (COMMA :: tokens)
            | ')' :: xs -> loop xs (RIGHT_PAREN :: tokens)
            | _ :: xs -> loop xs (UNKNOWN :: tokens)

        loop input []

    let fold tokens =

        let rec loop acc list =
            match list with
            | IDENTIFIER(MUL) :: LEFT_PAREN :: NUMBER(n) :: COMMA :: NUMBER(m) :: RIGHT_PAREN :: tail ->
                loop (acc + n * m) tail
            | _ :: tail -> loop acc tail
            | [] -> acc

        loop 0 tokens

module S2 =

    type Mode =
        | ENABLED
        | DISABLED

    let fold tokens =

        let rec loop acc list mode =

            match list with
            | IDENTIFIER(MUL) :: LEFT_PAREN :: NUMBER(n) :: COMMA :: NUMBER(m) :: RIGHT_PAREN :: tail ->
                match mode with
                | Mode.ENABLED -> loop (acc + n * m) tail mode
                | Mode.DISABLED -> loop acc tail mode
            | IDENTIFIER(DO) :: tail -> loop acc tail Mode.ENABLED
            | IDENTIFIER(DONT) :: tail -> loop acc tail Mode.DISABLED
            | _ :: tail -> loop acc tail mode
            | [] -> acc

        loop 0 tokens Mode.ENABLED

let input = File.ReadLines "Datasets/Day03.txt"

printfn "%d" (input |> Parser.parse |> S1.tokenize |> S1.fold)
printfn "%d" (input |> Parser.parse |> S1.tokenize |> S2.fold)
