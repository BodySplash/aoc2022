module AoC2022.Day10

open FSharpPlus

type Command =
    | AddX of int
    | Noop

module Command =
    let parse c =
        match c with
        | Regex @"addx (-?\d*)" [ v ] -> AddX(int v)
        | "noop" -> Noop
        | _ -> failwith "unknown"

    let parseAll: string seq -> Command seq = Seq.map parse

module CPU =

    let private processCommand (acc: (int -> int) List) (c: Command) =
        match c with
        | Noop -> acc @ [ id ]
        | AddX value -> acc @ [ id; (+) value ]

    let private toCycle commands = commands |> Seq.fold processCommand []

    let toCycles commands =
        commands |> toCycle |> Seq.scan (fun reg f -> f reg) 1 |> Seq.toArray


module Signal =
    let strength c (cycles: int array) = cycles[c - 1] * c

    let sum (cycles: int seq) = cycles |> map strength |> Seq.sum

module CRT =

    let draw (cycles: int seq) =

        let draw' (pixels, pos) reg =
            let h = pos % 40

            if reg >= h - 1 && reg <= h + 1 then
                ("#" :: pixels, pos + 1)
            else
                ("." :: pixels, pos + 1)


        (([], 0), cycles)
        ||> fold draw'
        |> fst
        |> rev
        |> Collection.split 40
        |> map (String.concat "")
        |> String.concat "\n"

let private sample = __SOURCE_DIRECTORY__ + "/input.txt"

let round1Result =
    readRows sample
    |> Command.parseAll
    |> CPU.toCycles
    |> Signal.sum [ 20; 60; 100; 140; 180; 220 ]


let round2Result =
    readRows sample
    |> Command.parseAll
    |> CPU.toCycles
    |> CRT.draw