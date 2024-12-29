namespace Gomu.Tuples

open System

type Pair<'T> = Tuple<'T, 'T>

module Pair =

    let map (f: 'T -> 'U) (pair: Pair<'T>) = Pair(f (fst pair), f (snd pair))

    let reduce (reduction: 'T -> 'T -> 'T) pair =
        reduction (fst pair) (snd pair)

type Triple<'T> = Tuple<'T, 'T, 'T>

module Triple =

    let inline fst (a, _, _) = a
    let inline snd (_, b, _) = b
    let inline trd (_, _, c) = c

    let toSeq (triple: Triple<'T>) =
        seq {
            fst triple
            snd triple
            trd triple
        }

    let map (f: 'T -> 'U) (triple: Triple<'T>) =
        Triple(f (fst triple), f (snd triple), f (trd triple))

    let reduce (reduction: 'T -> 'T -> 'T) triple =
        reduction (fst triple) (snd triple) |> reduction (trd triple)

    let fold (folder: 'State -> 'T -> 'State) acc triple =
        toSeq triple |> Seq.fold folder acc
