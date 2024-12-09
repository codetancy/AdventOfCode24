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
                permutations' (n - 1) list
                |> List.map (fun perm -> el :: perm))
            
[<RequireQualifiedAccess>]
module Array2D =

    let mapAt index1 index2 transform (array: 'T[,]) =
        array[index1, index2] <- transform array[index1, index2]

    let find value (array: 'T[,]) =
        let rows = Array2D.length1 array
        let cols = Array2D.length2 array

        let indices =
            seq {
                for i in 0 .. rows - 1 do
                    for j in 0 .. cols - 1 do
                        if array[i, j] = value then
                            yield i, j
            }

        indices |> Seq.head

    let count predicate (array: 'T[,]) =
        let rows = Array2D.length1 array
        let cols = Array2D.length2 array

        let values =
            seq {
                for i in 0 .. rows - 1 do
                    for j in 0 .. cols - 1 do
                        yield array[i, j]
            }

        (0, values)
        ||> Seq.fold (fun acc el -> if predicate el then acc + 1 else acc)

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
