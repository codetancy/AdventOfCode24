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

    let startsWith needle haystack =

        let rec loop needle haystack =
            match needle, haystack with
            | x1 :: xs1, y1 :: ys1 -> if x1 = y1 then true else loop xs1 ys1
            | [], _ -> true
            | _, [] -> false

        loop needle haystack
