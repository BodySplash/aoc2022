module AoC2022.Day13


open System
open FSharpPlus

type Packet =
    | Value of int
    | List of Packet list

type PacketsPair = Packet * Packet

module Packet =
    let compare ((left, right): PacketsPair) =

        let rec compare' left right =
            match left, right with
            | Value l, Value r when l = r -> None
            | Value l, Value r -> Some(l < r)
            | Value _, List _ -> compare' (List [ left ]) right
            | List _, Value _ -> compare' left (List [ right ])
            | List l, List r ->
                let res =
                    (Seq.zip l r)
                    |> Seq.fold
                        (fun acc (l, r) ->
                            match acc with
                            | None -> compare' l r
                            | Some _ -> acc)
                        None

                match res with
                | Some _ -> res
                | None ->
                    if (l.Length = r.Length) then
                        None
                    else
                        Some(l.Length < r.Length)

        compare' left right


module Input =
    let parsePacket (value: string) =
        let rec parse' acc value' =
            match value' with
            | '[' :: rest ->
                let res, remaining = parse' [] rest
                let acc = (res :: acc)
                parse' acc remaining
            | ']' :: rest -> (List(acc |> rev), rest)
            | ',' :: rest -> parse' acc rest
            | [] -> (acc |> head, [])
            | v ->
                let p = v |> Seq.takeWhile (fun i -> i <> ',' && i <> ']') |> Array.ofSeq |> String
                let rest = List.drop p.Length value'
                parse' (Value(int p) :: acc) rest

        parse' [] (value |> toList) |> fst


    let parse (inputs: string seq) : Packet list =
        let rec parse' acc inputs' =
            match inputs' with
            | value :: rest when value = "" -> parse' acc rest
            | first :: rest ->
                let acc = parsePacket first :: acc
                parse' acc rest
            | [] -> acc

        let i = parse' [] (inputs |> toList)

        parse' [] (inputs |> toList) |> rev

    let parsePair (inputs: string seq) : PacketsPair list =

        inputs
        |> parse
        |> Seq.chunkBySize 2
        |> Seq.map (fun [| f; s |] -> (f, s))
        |> Seq.toList


let round1 v =
    v
    |> Input.parsePair
    |> map Packet.compare
    |> List.indexed
    |> List.map (fun (i, v) -> (i + 1, v))
    |> List.filter (snd >> (=) (Some true))
    |> fold (fun acc i -> acc + (fst i)) 0

let round2 v =
    let d1 = List [ List [ Value 2 ] ]
    let d2 = List [ List [ Value 6 ] ]

    v
    |> Input.parse
    |> (@) [ d1; d2 ]
    |> Seq.sortWith (fun l r ->
        match Packet.compare (l, r) with
        | Some true -> -1
        | Some false -> 1
        | None -> 0)
    |> Seq.indexed
    |> Seq.map (fun (i, v) -> (i + 1, v))
    |> Seq.filter (fun (_, v) -> v = d1 || v = d2)
    |> Seq.map fst
    |> Seq.fold (*) 1

let private path = __SOURCE_DIRECTORY__ + "/input.txt"

let inputs = readRows path
