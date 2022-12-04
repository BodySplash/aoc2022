module AoC2022.Day4

open FSharpPlus
open AoC2022.utils

module Input =
    let parseLine raw =
        let s =
            raw
            |> String.split [ "-"; "," ]
            |> Seq.map int
            |> Seq.chunkBySize 2
            |> Seq.map (fun e -> (Array.head e, Array.last e))

        (head s, Seq.last s)

module Sections =
    let private overlap (f1, f2) (o1, o2) = (min f2 o2) - (max f1 o1) + 1

    let overlaps (first, other) = (overlap first other) > 0

    let private size (a, b) = (b - a) + 1

    let anyContainsFully (first, other) =
        let overlapSize = (overlap first other)

        overlapSize = size other
        || overlapSize = size first


let private inputPath =
    __SOURCE_DIRECTORY__ + "/input.txt"

let round1 rows =
    rows
    |> Seq.map Input.parseLine
    |> Seq.filter Sections.anyContainsFully
    |> Seq.length

let round2 rows =
    rows
    |> Seq.map Input.parseLine
    |> Seq.filter Sections.overlaps
    |> Seq.length

let round1Result =
    readRows inputPath |> round1

let round2Result =
    readRows inputPath |> round2
