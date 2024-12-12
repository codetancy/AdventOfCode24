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

[<RequireQualifiedAccess>]
type Correction =
    | Enabled
    | Disabled

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

    let reorder (rules: Map<int, int list>) (pages: int list) =
        let sorter p1 p2 =
            match Map.tryFind p1 rules with
            | Some rules -> if List.contains p2 rules then -1 else 1
            | None -> 1

        List.sortWith sorter pages

    let print
        (followingRules: Map<int, int list>)
        (precedingRules: Map<int, int list>)
        (updates: int list list)
        (mode: Correction)
        =

        updates
        |> List.choose (fun update ->
            let orderedForwards = isOrdered followingRules update
            let orderedBackwards = isOrdered precedingRules (List.rev update)

            match orderedForwards && orderedBackwards with
            | true -> Some update
            | false ->
                match mode with
                | Correction.Disabled -> None
                | Correction.Enabled ->
                    let orderForwards = reorder followingRules

                    let orderBackwards =
                        List.rev >> reorder precedingRules >> List.rev

                    let ordered = update |> orderForwards |> orderBackwards

                    Some ordered)
        |> List.map List.middle
        |> List.sum


let x, y, z = File.ReadAllLines "Files/Problem.txt" |> Parser.parse

let s1 = Solution.print x y z Correction.Disabled
printfn $"{s1}"

let s2 = Solution.print x y z Correction.Enabled
printfn $"{s2 - s1}"
