open System.IO

open Common

open Gomu.Arrays
open Gomu.Vectors

module Parser =

    let parse (lines: string seq) =
        lines |> Seq.map (fun s -> s.ToCharArray()) |> array2D

type Harmonics =
    | Enabled
    | Disabled

module Solution =

    let locateAntennas (grid: char[,]) =
        Array2D.toSeq grid
        |> Seq.filter (function
            | _, '.' -> false
            | _ -> true)
        |> Seq.groupBy snd
        |> Seq.map (fun (key, els) -> key, els |> Seq.map fst |> List.ofSeq)
        |> Map

    let uniqueAntinodes
        (grid: char[,])
        (antenna: Map<char, Vector2i list>)
        (harmonics: Harmonics)
        =

        let rec getAntinodes source displacement antinodes =
            let candidate = source + displacement

            match candidate with
            | Array2D.InBounds grid antinode ->
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
