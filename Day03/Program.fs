module Day03

open System
open System.IO

open Common

module Parser =

    let parse (lines: string seq) =
        lines |> Seq.map (fun s -> s.ToCharArray()) |> Seq.concat |> List.ofSeq

type Keyword = | MUL

type Token =
    | IDENTIFIER of Keyword
    | NUMBER of int
    | COMMA
    | LEFT_PAREN
    | RIGHT_PAREN
    | UNKNOWN

module S1 =

    let tokenize input =

        let rec loop input tokens =
            match input with
            | [] -> List.rev tokens
            | xs when Char.IsDigit(List.head xs) ->
                let number, rest = List.span Char.IsDigit xs
                let number = number |> Array.ofList |> String |> int
                loop rest (NUMBER(number) :: tokens)
            | 'm' :: 'u' :: 'l' :: rest -> loop rest (IDENTIFIER(MUL) :: tokens)
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

printfn
    "%d"
    (File.ReadLines "Files/Program.txt"
     |> Parser.parse
     |> S1.tokenize
     |> S1.fold)
