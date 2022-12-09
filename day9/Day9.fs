module AoC2022.Day9

open System.Text.RegularExpressions
open FSharpPlus
open AoC2022.utils

type Direction =
    | RIGHT
    | DOWN
    | LEFT
    | UP

type Move = Direction * int
let allDirections = [ RIGHT; DOWN; LEFT; UP ]

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
        let r = Regex @"(.) (\d+)"
        let m = r.Match v

        match m.Groups[1].Value with
        | "D" -> (Direction.DOWN, int m.Groups[2].Value)
        | "U" -> (Direction.UP, int m.Groups[2].Value)
        | "L" -> (Direction.LEFT, int m.Groups[2].Value)
        | "R" -> (Direction.RIGHT, int m.Groups[2].Value)

    let parse s : Move seq = s |> Seq.map parseOne

type Position = int * int

type Rope = Position * Position

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

    let findNextTail (head: Position) (tail: Position) =
        let p =
            match head, tail with
            | (hx, hy), (tx, ty) when hx = tx && hy > ty -> (tx, ty + 1)
            | (hx, hy), (tx, ty) when hx = tx && hy < ty -> (tx, ty - 1)
            | (hx, hy), (tx, ty) when hy = ty && hx > tx -> (tx + 1, ty)
            | (hx, hy), (tx, ty) when hy = ty && hx < tx -> (tx - 1, ty)
            | _ ->
                let possibleMoves =
                    (Position.neighbours head)
                    |> Set.ofList
                    |> Set.intersect (Position.neighbours tail |> Set.ofList)

                possibleMoves
                |> filter (fun (x, y) -> x = (fst head) || y = (snd head))
                |> Seq.head

        p


    let move ((head, tail): Rope) (d: Direction) =
        let newHead = Position.move head d

        if (Position.touching newHead tail) then
            (newHead, tail)
        else
            (newHead, (findNextTail newHead tail))

    let trail (r: Rope) (moves: Move seq) =
        let flattenMoves =
            moves |> Seq.map (fun (d, steps) -> Seq.replicate steps d) |> Seq.concat

        flattenMoves |> Seq.scan move r

let private inputPath = __SOURCE_DIRECTORY__ + "/input.txt"

let round1 i =
    i
    |> Move.parse
    |> Rope.trail ((0, 0), (0, 0))
    |> Seq.map snd
    |> Set.ofSeq
    |> Seq.length

let round1Result = readRows inputPath |> round1