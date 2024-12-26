module Common

open System
open System.Numerics
open Microsoft.FSharp.Core
open System.IO


[<RequireQualifiedAccess>]
module Option =
    let toOption value bool =
        match bool with
        | true -> Some value
        | false -> None

    let inline fromTuple (tuple: bool * ^T) =
        match tuple with
        | true, value -> Some value
        | false, _ -> None

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

    let rec combination n ls =
        match n, ls with
        | 0, _ -> [ [] ]
        | _, [] -> []
        | n, head :: tail ->
            let c1 =
                combination (n - 1) tail
                |> List.map (fun subCombination -> head :: subCombination)

            let c2 = combination n tail
            c1 @ c2

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


[<RequireQualifiedAccess>]
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

    let rec print (formatter: 'T -> string) (source: 'T seq) =
        printf "[ "
        source |> Seq.iter (fun el -> printf $"{formatter el} ")
        printfn "]"

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

    /// Checks whether the given indices are within the given Array's bounds
    let inBounds (index1, index2) (array: 'T[,]) =
        (index1 >= 0 && index1 < Array2D.length1 array)
        && (index2 >= 0 && index2 < Array2D.length2 array)

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

    let groupBy projection (array: 'T[,]) =
        toSeq array |> Seq.map snd |> Seq.groupBy projection

    let groupBy' projection (array: 'T[,]) =
        toSeq array |> Seq.groupBy projection

    let filter predicate (array: 'T[,]) =
        toSeq array |> Seq.filter (fun ((i, j), value) -> predicate value)

    let inline (|InBounds|_|) (array: 'T[,]) (y, x) =
        match inBounds (y, x) array with
        | true -> Some(y, x)
        | false -> None

[<RequireQualifiedAccess>]
module Array =

    let span predicate arr =
        let prefix = Array.takeWhile predicate arr
        let rest = Array.skip (Array.length prefix) arr
        prefix, rest

    let lens idx =
        fun arr -> Array.get arr idx, fun value arr -> Array.set arr idx value

    /// Updates the array swapping the elements at the given indices
    let swap idx1 idx2 (array: 'T[]) =
        let aux = array[idx1]
        array[idx1] <- array[idx2]
        array[idx2] <- aux

[<RequireQualifiedAccess>]
module Char =

    let toInt (c: char) = c - '0' |> int

[<RequireQualifiedAccess>]
module Int =

    let toChar (n: int) = char <| n + int '0'

    let inline inc (n: ^T) : ^T = n + LanguagePrimitives.GenericOne

    let inline dec (n: ^T) : ^T = n - LanguagePrimitives.GenericOne

    let inline isPositive (n: ^T) = n > LanguagePrimitives.GenericZero

    let inline isNegative (n: ^T) = n < LanguagePrimitives.GenericZero

    let inline isEven (n: ^T) =
        n &&& LanguagePrimitives.GenericOne = LanguagePrimitives.GenericZero

    let inline isOdd (n: ^T) =
        n &&& LanguagePrimitives.GenericOne = LanguagePrimitives.GenericOne

    let inline (|Even|Odd|) (n: ^T) = if isEven n then Even n else Odd n
    
    let inline digits (n: ^T) =
        if n = LanguagePrimitives.GenericZero then
            1
        else
            (n |> abs |> float |> log10 |> floor |> int) + 1

    let inline (|Positive|Negative|Zero|) (n: ^T) =
        if isPositive n then Positive
        elif isNegative n then Negative
        else Zero

[<RequireQualifiedAccess>]
module Vector2 =

    module Patterns =

        let (|In|_|) (array: 'T[,]) (vector: Vector2) =
            let i, j = int vector.Y, int vector.X

            match Array2D.inBounds (i, j) array with
            | true -> Some vector
            | false -> None

[<RequireQualifiedAccess>]
module File =

    /// Opens an existing UTF-8 encoded text file and returns a stream of
    /// characters
    let stream path =
        let reader = File.OpenText path

        seq {
            while not reader.EndOfStream do
                yield reader.Read()
        }

[<RequireQualifiedAccess>]
module Memory =

    /// Swaps the content of two Memory<'T> objects. Both `dest` and `source`
    /// must have the same length to ensure proper swapping.
    let swap (dest: Memory<'T>) (source: Memory<'T>) =
        let aux = Array.zeroCreate dest.Length
        dest.CopyTo(aux.AsMemory())
        source.CopyTo(dest)
        aux.AsMemory().CopyTo(source)
