open System
open System.IO

module Parser =
    let parseLine (line: string) =
        line.Split " " |> Array.map int |> List.ofArray

    let parse (lines: string seq) =
        lines |> Seq.map parseLine |> List.ofSeq

type Safety =
    | Safe
    | Unsafe

type Dampening =
    | On
    | Off

module Boolean =

    let toSafety bool =
        match bool with
        | true -> Safe
        | false -> Unsafe

module S1 =

    let isSafe safeness =
        match safeness with
        | Safety.Safe -> true
        | Safety.Unsafe -> false

    let between (lo, hi) n = lo <= n && n <= hi

    let withinBoundary differences =
        Seq.map Int32.Abs differences |> Seq.forall (between (1, 3))

    let isMonotonic differences =
        let ordering =
            match List.head differences > 0 with
            | true -> (fun x -> x > 0)
            | false -> (fun x -> x < 0)

        Seq.forall ordering differences
        
    let rec safetyOf report dampening =
        let differences =
            report |> List.pairwise |> List.map (fun (x, y) -> x - y)

        let isSafe = withinBoundary differences && isMonotonic differences

        match isSafe with
        | true -> Safety.Safe
        | false ->
            match dampening with
            | Dampening.Off -> Safety.Unsafe
            | Dampening.On ->
                Seq.init (List.length report) id
                |> Seq.map (fun index -> report |> List.removeAt index)
                |> Seq.exists (fun levels ->
                    safetyOf levels Dampening.Off = Safety.Safe)
                |> Boolean.toSafety

    let countSafe reports dampening =
        reports
        |> List.filter (fun report -> safetyOf report dampening |> isSafe)
        |> List.length

    let solution parse reports = countSafe (parse reports) Dampening.Off

module S2 =

    let solution parse reports =
        S1.countSafe (parse reports) Dampening.On

let input = File.ReadLines("Files/Reports.txt")

S1.solution Parser.parse input |> printfn "%d"
S2.solution Parser.parse input |> printfn "%d"
