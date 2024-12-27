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

type Harmonics =
    | Enabled
    | Disabled

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

    let uniqueAntinodes
        grid
        (antenna: Map<char, Vector2 list>)
        (harmonics: Harmonics)
        =

        let rec getAntinodes
            (source: Vector2)
            (displacement: Vector2)
            (antinodes: Vector2 list)
            =
            let candidate = source + displacement

            match candidate with
            | Vector2.In grid antinode ->
                match harmonics with
                | Enabled ->
                    antinode :: (getAntinodes antinode displacement antinodes)
                | Disabled -> antinode :: antinodes
            | _ -> antinodes

        let antinodesByAntenna =
            antenna
            |> Map.map (fun _ locations ->
                locations
                |> List.combination 2
                |> List.collect List.permutations
                |> List.map (fun vectors -> vectors[0], vectors[1])
                |> List.collect (fun (v1, v2) ->
                    let source =
                        match harmonics with
                        | Enabled -> v1
                        | Disabled -> v2

                    getAntinodes source (v2 - v1) []))

        Map.toSeq antinodesByAntenna
        |> Seq.collect snd
        |> Seq.distinct
        |> Seq.length


let grid = File.ReadAllLines "Datasets/Day08.txt" |> Parser.parse
let antenas = Solution.locateAntennas grid

let antinodes = Solution.uniqueAntinodes grid antenas Harmonics.Disabled
printfn $"{antinodes}"

let harmonics = Solution.uniqueAntinodes grid antenas Harmonics.Enabled
printfn $"{harmonics}"
