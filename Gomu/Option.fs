namespace Gomu.Option

module Option =
    let inline fromTuple (tuple: bool * ^T) =
        match tuple with
        | true, value -> Some value
        | false, _ -> None