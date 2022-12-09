module AoC2022.Day9

open System.Text.RegularExpressions
open FSharpPlus
open AoC2022.utils

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
        let r = Regex @"(.) (\d+)"
        let m = r.Match v

        match m.Groups[1].Value with
        | "D" -> (Direction.DOWN, int m.Groups[2].Value)
        | "U" -> (Direction.UP, int m.Groups[2].Value)
        | "L" -> (Direction.LEFT, int m.Groups[2].Value)
        | "R" -> (Direction.RIGHT, int m.Groups[2].Value)
        | _ -> failwith "nop"

    let parse s : Move seq = s |> Seq.map parseOne

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

            if Set.count possibleMoves = 1 then
                possibleMoves |> Set.toList |> List.head
            else
                possibleMoves
                |> filter (fun (x, y) -> x = (fst head) || y = (snd head))
                |> Seq.head




    let move ((head, tail): Rope) (d: Direction) =
        let rec moveTail' tail' curr acc =
            match tail' with
            | k :: rest ->
                let newK =
                    if (Position.touching curr k) then
                        k
                    else
                        (findNextTail curr k)

                moveTail' rest newK (newK :: acc)
            | [] -> acc

        let newHead = Position.move head d
        let newTail = (moveTail' tail newHead []) |> List.rev
        (newHead, newTail)

    let trail (r: Rope) (moves: Move seq) =
        let flattenMoves =
            moves |> Seq.map (fun (d, steps) -> Seq.replicate steps d) |> Seq.concat

        flattenMoves |> Seq.scan move r

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
