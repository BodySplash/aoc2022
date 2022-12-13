module AoC2022.Day12

open System.Collections.Generic
open FSharpPlus

type Position = int * int

type Direction =
    | Up
    | Down
    | Left
    | Right

module Position =
    let withCol col pos : Position = (fst pos, col)

    let withRow row pos : Position = (row, snd pos)

    let moveToNextRow (row, _) : Position = (row + 1, 0)

    let move pos d : Position =
        match d, pos with
        | Up, (row, col) -> (row - 1, col)
        | Down, (row, col) -> (row + 1, col)
        | Left, (row, col) -> (row, col - 1)
        | Right, (row, col) -> (row, col + 1)


type Plan =
    { startPos: Position; endPos: Position }

type Path = Position list

let emptyPlan = { startPos = (0, 0); endPos = (0, 0) }

type World = char[,]

module World =

    let allPosWithValue (world: World) (value: char) =
        seq {
            for row = 0 to world.GetUpperBound(0) do
                for col = 0 to world.GetUpperBound(1) do
                    if world[row, col] = value then
                        yield (row, col)
        }

module Input =

    let parse (data: string seq) : Plan * World =

        let withStart (plan: Plan) (row: char[]) (pos: Position) =
            match Array.tryFindIndex ((=) 'S') row with
            | Some i ->
                row[i] <- 'a'
                ({ plan with startPos = (Position.withCol i pos) }, row)
            | None -> (plan, row)

        let withEnd (plan: Plan) (row: char[]) (pos: Position) =
            match Array.tryFindIndex ((=) 'E') row with
            | Some i ->
                row[i] <- 'z'
                ({ plan with endPos = (Position.withCol i pos) }, row)
            | None -> (plan, row)

        let parse' ((plan, pos, world): Plan * Position * char[][]) (line: string) =
            let row = line.ToCharArray()
            let plan, row = withStart plan row pos
            let plan, row = withEnd plan row pos
            let world = Array.append world [| row |]
            (plan, Position.moveToNextRow pos, world)

        let plan, _, map = ((emptyPlan, (0, 0), [||]), data) ||> Seq.fold parse'
        (plan, array2D map)

module PathFinder =

    let private valueAt (world: World) ((row, col): Position) = world[row, col]

    let private neighbours (world: World) (position: Position) =
        [ Direction.Up; Direction.Down; Direction.Left; Direction.Right ]
        |> map (Position.move position)
        |> filter (fun (row, col) ->
            row >= 0
            && row <= (world.GetUpperBound 0)
            && col >= 0
            && col <= (world.GetUpperBound 1))
        |> map (fun p -> (p, world[fst p, snd p]))

    let private exclude (currentValue: char) (neighbours: (Position * char) seq) =
        neighbours
        |> filter (fun (_, value) -> (int value) <= (int currentValue + 1))
        |> map fst

    let findShortest (plan: Plan) (world: World) : Path Option =

        let neighbours = neighbours world


        let visited: IList<Position> = List()
        visited.Add plan.startPos
        let work: Queue<Position list> = Queue()
        work.Enqueue([ plan.startPos ])

        let mutable result = None

        while work.Count <> 0 do
            let currentPath = work.Dequeue()
            let currentNode = currentPath |> head

            if currentNode = plan.endPos then
                work.Clear()
                result <- Some currentPath
            else
                let n =
                    neighbours currentNode
                    |> exclude (world[fst currentNode, snd currentNode])
                    |> filter (fun p -> not (visited.Contains(p)))
                    |> toList

                n |> iter visited.Add
                n |> map (fun i -> i :: currentPath) |> iter work.Enqueue

        result

let private sample = __SOURCE_DIRECTORY__ + "/input.txt"

let inputs = readRows sample

let round1 s =
    s
    |> Input.parse
    ||> PathFinder.findShortest
    |> map (fun i -> i.Length)
    |> map (fun v -> v - 1)

let round2 s =
    let plan, world = Input.parse s
    World.allPosWithValue world 'a'
    |> Seq.map (fun p -> {plan with startPos = p})
    |> Seq.toArray
    |> Array.Parallel.map (fun plan ->
        printfn $"Looking for {plan.startPos} to {plan.endPos}"
        PathFinder.findShortest plan world)
    |> Seq.choose id
    |> map (fun i -> i.Length)
    |> sort
    |> Seq.map (fun v -> v - 1)
    |> tryHead
    