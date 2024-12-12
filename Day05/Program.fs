open System
open System.IO

open Common

module Parser =

    let parseRule (line: string) =
        match line.Split('|') with
        | [| n; m |] -> int n, int m
        | _ -> failwith "Page ordering not in the right format"

    let parseRules (lines: string seq) =
        let rules = lines |> Seq.map parseRule

        let parse grouper selector rules =
            rules
            |> Seq.groupBy grouper
            |> Map
            |> Map.map (fun _ values ->
                values |> Seq.map selector |> List.ofSeq)

        parse fst snd rules, parse snd fst rules

    let parseUpdate (line: string) =
        line.Split(',') |> Array.map int |> List.ofArray

    let parseUpdates (lines: string seq) =
        lines |> Seq.map parseUpdate |> List.ofSeq

    let parse (lines: string seq) =
        let orderingAndOrders =
            Seq.split String.IsNullOrWhiteSpace lines |> List.ofSeq

        match orderingAndOrders with
        | [ rawRules; rawUpdates ] ->
            let followingRules, precedingRules = parseRules rawRules
            let updates = parseUpdates rawUpdates
            followingRules, precedingRules, updates
        | _ -> failwith "There's not exacly one empty line in the input file"

module Solution =

    let rec isOrdered (pageRules: Map<int, int list>) (update: int list) =

        match update with
        | []
        | [ _ ] -> true
        | page :: nextPages ->
            match Map.tryFind page pageRules with
            | None -> isOrdered pageRules nextPages
            | Some rules ->
                let valid =
                    List.forall (fun page -> List.contains page rules) nextPages

                match valid with
                | true -> isOrdered pageRules nextPages
                | false -> false

    let print
        (followingRules: Map<int, int list>)
        (precedingRules: Map<int, int list>)
        (updates: int list list)
        =

        updates
        |> List.choose (fun update ->
            let orderedForwards = isOrdered followingRules update
            let orderedBackwards = isOrdered precedingRules (List.rev update)

            match orderedForwards && orderedBackwards with
            | true -> Some update
            | false -> None)
        |> List.map List.middle
        |> List.sum


let x, y, z = File.ReadAllLines "Files/Problem.txt" |> Parser.parse
let sum = Solution.print x y z

printfn $"{sum}"
