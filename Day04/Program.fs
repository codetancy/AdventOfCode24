open System.IO

open Common
open Gomu.Vectors
open Gomu.Arrays
open Gomu.Tuples

module Parser =

    let parseLine (line: string) = line.ToCharArray()

    let parse lines = lines |> Seq.map parseLine |> array2D

module State =
    let next =
        function
        | 'X' -> 'M'
        | 'M' -> 'A'
        | 'A' -> 'S'
        | c -> failwith $"Unrecognized character {c}"

module S1 =
    let orthogonalComponents = Orthogonal.Values |> Seq.map Offset.ofOrthogonal

    let diagonalComponents =
        Orthogonal.Diagonals
        |> Seq.map (fun components ->
            components |> Pair.map Offset.ofOrthogonal |> Pair.reduce (+))

    let allDirections = Seq.concat [ orthogonalComponents; diagonalComponents ]

    let solve (wordSearch: char[,]) =

        let rec search p0 d state =
            match state with
            | 'S' -> true
            | _ ->
                match p0 + d with
                | Array2D.InBounds wordSearch p1 ->
                    match State.next state with
                    | nextState when nextState = wordSearch[p1.X, p1.Y] ->
                        search p1 d nextState
                    | _ -> false

                | outOfBounds -> false

        (0, wordSearch)
        ||> Array2D.fold (fun acc (p, value) ->
            match value with
            | 'X' ->
                let solutions =
                    allDirections
                    |> Seq.filter (fun d -> search p d 'X')
                    |> Seq.length

                acc + solutions
            | _ -> acc)

module S2 =

    type Region =
        | In
        | Out

    let boundsOf grid point =
        match point with
        | Array2D.InBounds grid _ -> Region.In
        | _ -> Region.Out

    let oppositeCornersPairs =
        [ (North, West), (South, East); (South, West), (North, East) ]

    let oppositeCornersComponents =
        oppositeCornersPairs
        |> Seq.map (fun corners ->
            corners
            |> Pair.map (fun components ->
                // Adding the orthogonal components to get the diagonal displacement
                components |> Pair.map Offset.ofOrthogonal |> Pair.reduce (+)))
        |> List.ofSeq

    let solve (wordSearch: char[,]) =

        (0, wordSearch)
        ||> Array2D.fold (fun acc (p0, value) ->
            match value with
            | 'A' ->
                let oppositeCorners =
                    oppositeCornersComponents
                    |> Seq.map (Pair.map (fun d -> p0 + d))

                let result =
                    oppositeCorners
                    |> Seq.forall (fun corners ->
                        match Pair.map (boundsOf wordSearch) corners with
                        | Region.In, Region.In ->
                            corners
                            |> Pair.map (fun p -> wordSearch[p.X, p.Y])
                            |> function
                                | 'S', 'M'
                                | 'M', 'S' -> true
                                | _ -> false
                        | _ -> false)

                match result with
                | true -> acc + 1
                | false -> acc
            | _ -> acc)

let input = File.ReadLines "Datasets/Day04.txt" |> Parser.parse

let xmas = S1.solve input
printfn $"{xmas}"

let x_mas = S2.solve input
printfn $"{x_mas}"
