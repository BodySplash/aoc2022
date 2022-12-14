module AoC2022.Day6

open AoC2022.utils
open FSharpPlus

let private inputPath =
    __SOURCE_DIRECTORY__ + "/input.txt"


let findMarker count input = 
    input
        |> Seq.windowed count
        |> map (Set.ofArray >> Set.count)
        |> Seq.findIndex ((=) count)
        |> (+) count
let findStartPacket = findMarker 4
let findStartMessage = findMarker 14

let inputs = readRows inputPath

let round1Result =
    inputs
    |> map findStartPacket
    |> Seq.tryExactlyOne

let round2Result =
    inputs
    |> Seq.map findStartMessage
    |> Seq.tryExactlyOne