module AoC2022.Day1

open System
open AoC2022.utils
open FSharpPlus

[<Literal>]
let private inputPath =
    __SOURCE_DIRECTORY__ + "/input.csv"


let (|Calories|_|): _ -> UInt32 option =
    tryParse

let rows =
    readRows inputPath
    |> Seq.map (fun l ->
        match l with
        | Calories v -> Some v
        | _ -> None)
    |> Seq.toList


let rec groupElves (data: UInt32 option list) (acc: UInt32 list list) =
    match data, acc with
    | Some calories :: rest, current :: tail ->
        let newCurrent = calories :: current
        groupElves rest (newCurrent :: tail)
    | None :: rest, _ -> groupElves rest ([] :: acc)
    | _ -> acc

let summedCalories =
    groupElves rows [ [] ]
    |> Seq.map Seq.sum
    |> Seq.sortDescending

let round1 = summedCalories |> Seq.head

let round2 =
    summedCalories |> Seq.take 3 |> Seq.sum
