module AoC2022.Day9

open System.Text.RegularExpressions
open FSharpPlus
open AoC2022.utils
open Microsoft.FSharp.Core

type Position = int * int

type Rope = Position * Position list

type Direction =
    | RIGHT
    | DOWN
    | LEFT
    | UP

type Move = Direction * int

let private allMoves =
    [ [ RIGHT ]
      [ DOWN ]
      [ LEFT ]
      [ UP ]
      [ RIGHT; UP ]
      [ RIGHT; DOWN ]
      [ LEFT; UP ]
      [ LEFT; DOWN ] ]

module Move =

    let private parseOne v =
        match v with
        | Regex @"(.) (\d+)" [ c; count ] ->
            match c with
            | "D" -> (Direction.DOWN, int count)
            | "U" -> (Direction.UP, int count)
            | "L" -> (Direction.LEFT, int count)
            | "R" -> (Direction.RIGHT, int count)
            | _ -> failwith "nop"
        | _ -> failwith "Nop"


    let parse s : Move seq = s |> Seq.map parseOne

    let expand moves =
        moves |> Seq.map (fun (dir, steps) -> Seq.replicate steps dir) |> Seq.concat

module Position =
    let move (x, y) d =
        match d with
        | UP -> (x, y + 1)
        | DOWN -> (x, y - 1)
        | RIGHT -> (x + 1, y)
        | LEFT -> (x - 1, y)

    let neighbours p =
        allMoves |> (List.map (List.fold move p))

    let touching p1 p2 =
        let n = neighbours p1
        p1 = p2 || Seq.contains p2 n

module Rope =

    let moveTail (head: Position) (tail: Position) =
        let possibleMoves =
            (Position.neighbours head)
            |> Set.ofList
            |> Set.intersect (Position.neighbours tail |> Set.ofList)

        possibleMoves
        |> filter (fun (x, y) -> x = (fst head) || y = (snd head))
        |> Seq.tryHead
        |> Option.defaultValue (possibleMoves |> Seq.head)

    let move ((head, tail): Rope) (d: Direction) =
        let rec move' head' tail' acc =
            match tail' with
            | curr :: rest ->
                let movedTail =
                    if (Position.touching head' curr) then
                        curr
                    else
                        (moveTail head' curr)

                move' movedTail rest (movedTail :: acc)
            | [] -> acc

        let newHead = Position.move head d
        let newTail = (move' newHead tail []) |> List.rev
        (newHead, newTail)

    let trail (rope: Rope) (moves: Move seq) =
        moves |> Move.expand |> Seq.scan move rope

let private inputPath = __SOURCE_DIRECTORY__ + "/input.txt"

let round (initial: Rope) (i: string seq) =
    i
    |> Move.parse
    |> Rope.trail initial
    |> Seq.map snd
    |> Seq.map (List.rev >> List.head)
    |> Set.ofSeq
    |> Seq.length

let round1 i = i |> round ((0, 0), [ (0, 0) ])

let round2 i =
    i |> round (((0, 0), (Seq.replicate 9 (0, 0)) |> Seq.toList))

let round1Result = readRows inputPath |> round1
let round2Result = readRows inputPath |> round2
