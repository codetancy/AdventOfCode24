open System
open System.Collections.Generic
open System.IO

open System.Numerics
open Common

module Parser =

    let parse (lines: string seq) =
        lines |> Seq.map (fun s -> s.ToCharArray()) |> array2D

module Vector2 =

    let create (y, x) = Vector2(float32 x, float32 y)

module Solution =

    let locateAntennas grid =
        Array2D.toSeq grid
        |> Seq.filter (function
            | _, '.' -> false
            | _ -> true)
        |> Seq.groupBy snd
        |> Seq.map (fun (key, els) ->
            // (i, j) -> { X = j, Y = i }
            let vectors = els |> Seq.map (fst >> Vector2.create) |> List.ofSeq
            key, vectors)
        |> Map

    let solve grid (antenna: Map<char, Vector2 list>) =
        let antinodesByAntenna =
            antenna
            |> Map.map (fun _ locations ->
                locations
                |> List.combination 2
                |> List.collect List.permutations
                |> List.map (fun vectors -> vectors[0], vectors[1])
                |> List.collect (fun (v1, v2) ->
                    let d = v2 - v1
                    let v3 = v2 + d

                    match int v3.Y, int v3.X with
                    | Patterns.InBounds grid _ -> [ v3 ]
                    | _ -> []))

        let unique =
            Map.toSeq antinodesByAntenna |> Seq.collect snd |> Seq.distinct

        Seq.length unique



let grid = File.ReadAllLines "Files/Problem.txt" |> Parser.parse
let antenas = Solution.locateAntennas grid
let uniqueAntinodes = Solution.solve grid antenas

printfn $"{uniqueAntinodes}"
