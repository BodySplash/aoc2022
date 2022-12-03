module AoC2022.Day3

open AoC2022.utils
open FSharpPlus

let priorities =
    Seq.zip
        (Seq.concat [ [ 'a' .. 'z' ]
                      [ 'A' .. 'Z' ] ])
        (Seq.initInfinite ((+) 1))
    |> Map.ofSeq

let split (elems: int) b =
    let numberOfChunks = (Seq.length b) / elems
    b |> Seq.splitInto numberOfChunks
let splitRuckSack (value: string) =
    split (value.Length / 2) (value.ToCharArray())

let findSharedItems (bags: char [] seq) =
    Set.intersectMany (bags |> Seq.map Set.ofSeq)


let sumSharedItems (e: char seq) =
    e |> Seq.map (fun i -> priorities[i]) |> Seq.sum

[<Literal>]
let private inputPath =
    __SOURCE_DIRECTORY__ + "/input.txt"

let round1 v =
    v
    |> Seq.map splitRuckSack
    |> Seq.map findSharedItems
    |> Seq.map sumSharedItems
    |> Seq.sum

let round2 (v: string seq) =
    v
    |> Seq.map (fun v -> v.ToCharArray())
    |> split 3
    |> Seq.map findSharedItems
    |> Seq.map Seq.tryExactlyOne
    |> sequence
    |> Option.apply (Some sumSharedItems)

let round1Result =
    readRows inputPath |> round1

let round2Result =
    readRows inputPath |> round2