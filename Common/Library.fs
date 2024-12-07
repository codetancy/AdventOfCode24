namespace Common

module Option =
    let toOption value bool =
        match bool with
        | true -> Some value
        | false -> None

[<RequireQualifiedAccess>]
module List =

    let span predicate list =

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
