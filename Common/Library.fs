namespace Common

open Microsoft.FSharp.Core

module Option =
    let toOption value bool =
        match bool with
        | true -> Some value
        | false -> None

[<RequireQualifiedAccess>]
module List =

    let span predicate list =
        let prefix = List.takeWhile predicate list
        let rest = List.skip (List.length prefix) list
        prefix, rest

    let span' predicate list =

        let rec loop acc list =
            match list with
            | [] -> List.rev acc, []
            | x :: xs when predicate x -> loop (x :: acc) xs
            | xs -> List.rev acc, xs

        loop [] list

    let startsWith prefix list =
        if List.length prefix > List.length list then
            false
        else
            List.take (List.length prefix) list = prefix

    let after prefix list =
        if startsWith prefix list then
            List.skip (List.length prefix) list
        else
            failwith "List must start with prefix"

    let tryAfter prefix list =
        if startsWith prefix list then
            Some <| List.skip (List.length prefix) list
        else
            None

    let rec permutations (list: 'T list) =
        match list with
        | [] -> [ [] ]
        | [ x ] -> [ [ x ] ]
        | list ->
            list
            |> List.collect (fun el ->
                let rest = List.filter (fun x -> not (x = el)) list
                permutations rest |> List.map (fun perm -> el :: perm))

    let rec permutations' n (list: 'T list) =
        match n with
        | 0 -> [ [] ]
        | n ->
            list
            |> List.collect (fun el ->
                permutations' (n - 1) list |> List.map (fun perm -> el :: perm))

    let split (predicate: 'T -> bool) (source: 'T list) =

        let rec loop source output =
            if Seq.isEmpty source then
                List.rev output
            else
                let section = List.takeWhile (predicate >> not) source

                let remaining =
                    source
                    |> List.skip (List.length section)
                    |> List.skipWhile predicate

                loop remaining (section :: output)

        loop source []

    let middle ls =
        ls |> List.skip (List.length ls / 2) |> List.head


module Seq =

    /// Returns a sequence that, when iterated, yields elements of the underlying
    /// sequence in chunks separated by when the given predicate returns True
    let split (predicate: 'T -> bool) (source: 'T seq) =
        let rec loop (source: 'T seq) : seq<'T seq> =
            seq {
                if not (Seq.isEmpty source) then
                    let section = Seq.takeWhile (predicate >> not) source
                    yield section

                    let remaining =
                        source
                        |> Seq.skip (Seq.length section)
                        |> Seq.skipWhile predicate

                    yield! loop remaining
            }

        loop source

[<RequireQualifiedAccess>]
module Array2D =

    /// Views the given Array2D as an enumerable collection of pairs. The
    /// sequence will be ordered by the indices of each element
    let toSeq (array: 'T[,]) =
        seq {
            for i in 0 .. Array2D.length1 array - 1 do
                for j in 0 .. Array2D.length2 array - 1 do
                    (i, j), array[i, j]
        }

    let mapAt index1 index2 transform (array: 'T[,]) =
        array[index1, index2] <- transform array[index1, index2]

    /// Returns the indices of the first matching element
    let find value (array: 'T[,]) =
        toSeq array |> Seq.find (fun (_, el) -> el = value) |> fst

    /// Counts the number of elements for which the given predicate returns true
    let count predicate (array: 'T[,]) =
        toSeq array
        |> Seq.map snd
        |> (fun x -> (0, x))
        ||> Seq.fold (fun acc el -> if predicate el then acc + 1 else acc)

    let fold folder state (array: 'T[,]) = toSeq array |> Seq.fold folder state

    let groupBy (projection: 'T -> 'U) (array: 'T[,]) =
        toSeq array |> Seq.map snd |> Seq.groupBy projection

module Array =

    let span predicate arr =
        let prefix = Array.takeWhile predicate arr
        let rest = Array.skip (Array.length prefix) arr
        prefix, rest

module Int64 =

    let digits n =

        let rec digits' n count =
            if n / 10L = 0L then
                count
            else
                digits' (n / 10L) (count + 1)

        digits' (abs n) 1

module Patterns =

    let (|InBounds|_|) (floor: 'T[,]) (y, x) =
        if
            (y >= 0 && y < Array2D.length1 floor)
            && (x >= 0 && x < Array2D.length2 floor)
        then
            Some(y, x)
        else
            None

    let (|Even|Odd|) n = if n % 2 = 0 then Even else Odd
