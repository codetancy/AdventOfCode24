module Day03

open System
open System.IO

open Common

module Parser =

    let parse (lines: string seq) =
        lines |> Seq.map (fun s -> s.ToCharArray()) |> Seq.concat |> List.ofSeq

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
            | 'd' :: 'o' :: 'n' :: '\'' :: 't' :: rest ->
                loop rest (IDENTIFIER(DONT) :: tokens)
            | 'd' :: 'o' :: rest -> loop rest (IDENTIFIER(DO) :: tokens)
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

    let fold tokens =

        let rec loop acc list enabled =

            match list with
            | IDENTIFIER(MUL) :: LEFT_PAREN :: NUMBER(n) :: COMMA :: NUMBER(m) :: RIGHT_PAREN :: tail when
                enabled
                ->
                loop (acc + n * m) tail enabled
            | IDENTIFIER(DO) :: tail -> loop acc tail true
            | IDENTIFIER(DONT) :: tail -> loop acc tail false
            | _ :: tail -> loop acc tail enabled
            | [] -> acc

        loop 0 tokens true

let input = File.ReadLines "Files/Program.txt"

printfn "%d" (input |> Parser.parse |> S1.tokenize |> S1.fold)
printfn "%d" (input |> Parser.parse |> S1.tokenize |> S2.fold)
