module AoC2022.Day5

open System.Text.RegularExpressions
open FSharpPlus
open AoC2022.utils

type Move = { Number: int; From: int; To: int }

type Stacks = char list list



module Inputs =

    let rec private transpose xs =
        [ match xs with
          | [] -> failwith "cannot transpose a 0-by-n matrix"
          | [] :: _ -> ()
          | xs ->
              yield map head xs
              yield! transpose (map List.tail xs) ]

    let private parseStackLine s =
        s
        |> Seq.indexed
        |> filter (fst >> ((flip (%)) 4 >> (=) 1)) //(fun (i, _) -> i % 4 = 1)
        |> map snd
        |> toList

    let private (|Stack|_|) (input: string) =
        if (input.Contains "[") then
            Some(parseStackLine input)
        else
            None

    let private (|Instructions|_|) (input: string) =
        let pattern =
            @"move (\d*) from (\d*) to (\d*)"

        let m = Regex.Match(input, pattern)

        if m.Success then
            let number = m.Groups[1].Value |> int
            let from = m.Groups[2].Value |> int
            let toStack = m.Groups[3].Value |> int

            Some(
                { Number = number
                  From = from
                  To = toStack }
            )
        else
            None

    let parse (i: string seq) =
        let mutable stacks = []
        let mutable instructions = []
        let mutable finished = false
        let mutable i = i |> Seq.toList

        while not finished do
            match i with
            | [] -> finished <- true
            | head :: tail ->
                i <- tail

                match head with
                | Stack s -> stacks <- s :: stacks
                | Instructions i -> instructions <- i :: instructions
                | _ -> ()

        (stacks
         |> rev
         |> transpose
         |> map (filter ((<>) ' ')),
         instructions |> rev)

module Crane =

    let applyMove9001 (s: Stacks) (m: Move) : Stacks =
        let from = s[m.From - 1]
        let destination = s[m.To - 1]
        let elems = from[.. m.Number - 1]
        let newSource = from[m.Number ..]

        let newDest =
            List.concat [ elems; destination ]

        s
        |> List.setAt (m.From - 1) newSource
        |> List.setAt (m.To - 1) newDest

    let applyMove (s: Stacks) (m: Move) : Stacks =
        let from = s[m.From - 1]
        let destination = s[m.To - 1]

        let elems =
            from[.. m.Number - 1] |> List.rev

        let newSource = from[m.Number ..]

        let newDest =
            List.concat [ elems; destination ]

        s
        |> List.setAt (m.From - 1) newSource
        |> List.setAt (m.To - 1) newDest


    let rec private applyM (mover: Stacks -> Move -> Stacks) (s: Stacks) (m: Move list) : Stacks =
        match m with
        | move :: rest -> applyM mover (mover s move) rest
        | [] -> s

    let applyMoves = applyM applyMove

    let applyMoves9001 = applyM applyMove9001



let private inputPath =
    __SOURCE_DIRECTORY__ + "/input.txt"

let inputs =
    readRows inputPath |> Inputs.parse

let round1 (stacks, moves) =
    (Crane.applyMoves stacks moves)
    |> map List.head
    |> map string
    |> String.concat ""

let round2 (stacks, moves) =
    (Crane.applyMoves9001 stacks moves)
    |> map head
    |> map string
    |> String.concat ""

let round1Result = inputs |> round1

let round2Result = inputs |> round2
