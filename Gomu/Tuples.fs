namespace Gomu.Tuples

open System

type Pair<'T> = Tuple<'T, 'T>

module Pair =

    let map (f: 'T -> 'U) (pair: Pair<'T>) = Pair(f (fst pair), f (snd pair))

type Triple<'T> = Tuple<'T, 'T, 'T>

module Triple =

    let inline fst (a, _, _) = a
    let inline snd (_, b, _) = b
    let inline trd (_, _, c) = c

    let map (f: 'T -> 'U) (triple: Triple<'T>) =
        Triple(f (fst triple), f (snd triple), f (trd triple))
