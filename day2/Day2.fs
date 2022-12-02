module AoC2022.Day2

open AoC2022.utils
open FSharpPlus
open Microsoft.FSharp.Core

type Figure =
    | Rock = 1
    | Paper = 2
    | Scissors = 3

type Strategy =
    | Win
    | Lose
    | Draw

type Turn = Turn of Figure * Figure

type RealTurn = RealTurn of Figure * Strategy


module Figure = 
    let parse =
        function
        | "A"
        | "X" -> Some Figure.Rock
        | "B"
        | "Y" -> Some Figure.Paper
        | "C"
        | "Z" -> Some Figure.Scissors
        | _ -> None


module Strategy =
    let parse =
        function
        | "X" -> Some Strategy.Lose
        | "Y" -> Some Strategy.Draw
        | "Z" -> Some Strategy.Win
        | _ -> None

[<RequireQualifiedAccess>]
module Score =
    let private rawScore (t: Turn) =
        match t with
        | Turn (a, b) when a = b -> 3
        | Turn (Figure.Rock, Figure.Paper) -> 6
        | Turn (Figure.Paper, Figure.Scissors) -> 6
        | Turn (Figure.Scissors, Figure.Rock) -> 6
        | _ -> 0


    let private turnValues = function
        | Turn(f, f2) -> (f, f2)
    
    let fromTurn (turn: Turn) =
        let _, p2 = turnValues turn
        (int p2) + (rawScore turn)


[<RequireQualifiedAccess>]
module Round1 =
    let parseTurn (v: string) =
        let r =
            v
            |> String.split [ " " ]
            |> Seq.map Figure.parse
            |> Seq.toList
            |> sequence

        match r with
        | Some [ first; second ] -> Some(Turn(first, second))
        | _ -> None

    let parseTurns (v: string seq) =
        v
        |> Seq.map parseTurn
        |> sequence
        |> Option.map Seq.toList

[<RequireQualifiedAccess>]
module Round2 =

    let parseTurn (v: string) =
        let split =
            v |> String.split [ " " ] |> Seq.toList

        let r =
            match split with
            | [ f; s ] -> (Figure.parse f, Strategy.parse s)
            | _ -> (None, None)

        match r with
        | Some f, Some s -> Some(RealTurn(f, s))
        | _ -> None


    let private winMove =
        function
        | Figure.Paper -> Figure.Scissors
        | Figure.Scissors -> Figure.Rock
        | Figure.Rock -> Figure.Paper
        | _ -> failwith "Unknown"

    let private looseMove =
        function
        | Figure.Paper -> Figure.Rock
        | Figure.Scissors -> Figure.Paper
        | Figure.Rock -> Figure.Scissors
        | _ -> failwith "Unknown"

    let computeMove (t: RealTurn) =
        match t with
        | RealTurn (f, Win) -> Turn(f, winMove f)
        | RealTurn (f, Draw) -> Turn(f, f)
        | RealTurn (f, Lose) -> Turn(f, looseMove f)


    let parseTurns (v: string seq) =
        v
        |> Seq.map parseTurn
        |> sequence
        |> Option.map Seq.toList



[<Literal>]
let private inputPath =
    __SOURCE_DIRECTORY__ + "/input.txt"


let round1 =
    readRows inputPath
    |> Round1.parseTurns
    |> Option.apply (Option.Some((Seq.map Score.fromTurn) >> Seq.sum))

let round2 =
    readRows inputPath
    |> Round2.parseTurns
    |> Option.apply (
        Option.Some(
            Seq.map (Round2.computeMove >> Score.fromTurn)
            >> Seq.sum
        )
    )
