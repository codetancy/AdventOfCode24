open System
open System.IO

module S1 =
    let distance (x, y) = Int32.Abs(x - y)

    let totalDistance (l1, l2) =
        List.zip l1 l2 |> List.map distance |> List.sum

module S2 =
    let toHistogram (ls: int list) =
        List.groupBy id ls
        |> List.map (fun (n, ls') -> n, List.length ls')
        |> Map.ofList

    let similarityScore (hist: Map<int, int>) =
        (fun n -> n * (Map.tryFind n hist |> Option.defaultValue 0))

    let totalSimilarity (l1, l2) =
        l1 |> List.map (similarityScore (toHistogram l2)) |> List.sum

module Parser =
    let parseLine (line: string) =
        match line.Split "   " with
        | [| hs; ls |] -> Int32.Parse hs, Int32.Parse ls
        | _ -> failwith "Input line is not in the correct format"

    let parse (lines: string seq) =
        let l1, l2 = lines |> Seq.map parseLine |> List.ofSeq |> List.unzip
        List.sort l1, List.sort l2

let solution1 parser lines = S1.totalDistance (parser lines)

let solution2 parser lines = S2.totalSimilarity (parser lines)

let input = File.ReadLines("Datasets/Day01.txt")

printfn "%d" <| solution1 Parser.parse input
printfn "%d" <| solution2 Parser.parse input
