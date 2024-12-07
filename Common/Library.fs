namespace Common

module Option =
    let toOption value bool =
        match bool with
        | true -> Some value
        | false -> None

module List =
    
    let span predicate list =
        
        let rec loop acc list =
            match list with
            | [] -> List.rev acc, []
            | x :: xs when predicate x -> loop (x :: acc) xs
            | xs -> List.rev acc, xs
            
        loop [] list 