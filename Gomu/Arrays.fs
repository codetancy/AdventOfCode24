namespace Gomu.Arrays

open Gomu.Vectors

[<RequireQualifiedAccess>]
module Array2D =

    /// Views the given Array2D as an enumerable collection of pairs. The
    /// sequence will be ordered by the indices of each element
    let toSeq (array: 'T[,]) =
        seq {
            for i in 0 .. Array2D.length1 array - 1 do
                for j in 0 .. Array2D.length2 array - 1 do
                    { X = i; Y = j }, array[i, j]
        }

    /// Checks whether the given indices are within the given Array's bounds
    let inBounds coord (array: 'T[,]) =
        let between lo hi n = n > lo && n < hi

        (coord.X |> between -1 (Array2D.length1 array))
        && (coord.Y |> between -1 (Array2D.length2 array))

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
        toSeq array |> Seq.filter (fun (_, value) -> predicate value)

    let inline (|InBounds|_|) (array: 'T[,]) coordinates =
        match inBounds coordinates array with
        | true -> Some coordinates
        | false -> None
