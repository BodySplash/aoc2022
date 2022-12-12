module AoC2022.Day11

open FSharpPlus

type MonkeyTest =
    { predicate: int
      ifTrue: int
      ifFalse: int }

type Monkey =
    { items: uint64 list
      operation: uint64 -> uint64
      test: MonkeyTest }

let emptyMonkey =
    { items = []
      operation = id
      test =
        { predicate = 0
          ifTrue = 0
          ifFalse = 0 } }

module Input =
    let parse lines =

        let createOperation left op right : uint64 -> uint64 =
            let fn =
                match op with
                | "*" -> (*)
                | "+" -> (+)
                | _ -> failwith "not supported"

            match left, right with
            | "old", "old" -> (fun i -> fn i i)
            | "old", v -> (fun i -> fn i (uint64 v))
            | v, "old" -> (fun i -> fn (uint64 v) i)
            | _ -> failwith "not supported"

        let parse' acc line =
            match line, acc with
            | Regex @"Monkey \d+:" _, _ -> emptyMonkey :: acc
            | Regex @"Starting items: (.*)" [ items ], monkey :: otherMonkeys ->
                let parsed = items.Split ", " |> map uint64 |> Seq.toList
                { monkey with items = parsed } :: otherMonkeys
            | Regex @"Operation: new = ([^\s]+) (.) (.*)" [ left; op; right ], monkey :: otherMonkeys ->
                let parsed = createOperation left op right
                { monkey with operation = parsed } :: otherMonkeys
            | Regex @"Test: divisible by (\d+)" [ value ], monkey :: otherMonkeys ->
                { monkey with test = { monkey.test with predicate = int value } }
                :: otherMonkeys
            | Regex @"If ([^:]+): throw to monkey (\d+)" [ cond; value ], monkey :: otherMonkeys ->
                match cond with
                | "true" -> { monkey with test = { monkey.test with ifTrue = (int value) } }
                | "false" -> { monkey with test = { monkey.test with ifFalse = (int value) } }
                :: otherMonkeys
            | _ -> acc

        ([], lines) ||> Seq.fold parse' |> rev |> toArray

module Game =

    let evalMonkey (relief: uint64 -> uint64) (monkey: Monkey) : Monkey * (int * uint64) list =

        let rec eval' actions items =
            match items with
            | item :: others ->
                let newWorry = relief (monkey.operation item)

                let actions =
                    if  newWorry % uint64 monkey.test.predicate = 0UL then
                        (monkey.test.ifTrue, newWorry) :: actions
                    else
                        (monkey.test.ifFalse, newWorry) :: actions

                eval' actions others

            | [] -> actions |> rev

        let actions = eval' [] monkey.items
        ({ monkey with items = [] }, actions)

    let private applyActions (actions: (int * uint64) list) (monkeys: Monkey array) =
        (monkeys, actions)
        ||> fold (fun acc (index, worry) ->
            let monkey = { acc[index] with items = acc[index].items @ [ worry ] }
            Array.set acc index monkey
            acc)

    let private raiseBusiness key map value =
        Map.change
            key
            (function
            | Some v -> Some(value + v)
            | None -> Some value)
            map

    let private monkeyRound relief (monkeyBusiness: Map<int, uint64>, monkeys: Monkey array) monkeyNb =
        let monkeyBusiness =
            raiseBusiness monkeyNb monkeyBusiness (uint64 monkeys[monkeyNb].items.Length)

        let newMonkey, actions = evalMonkey relief monkeys[monkeyNb]
        Array.set monkeys monkeyNb newMonkey
        applyActions actions monkeys
        (monkeyBusiness, monkeys)

    let runAndEval relief (monkeyBusiness, monkeys: Monkey array) =
        ((monkeyBusiness, monkeys), [ 0 .. monkeys.Length - 1 ])
        ||> Seq.fold (monkeyRound relief)

    let runNRounds nbRounds relief (monkeys: Monkey array) =
        ((Map.empty, monkeys), [ 1..nbRounds ])
        ||> List.fold (fun acc _ -> runAndEval relief acc)
    
    let runNRoundsWithRelief nbRounds (monkeys: Monkey array) =
        runNRounds nbRounds (flip (/) 3UL) monkeys
        
    let runNRoundsWithoutRelief nbRounds (monkeys: Monkey array) =
        let factors = monkeys |> map (fun i -> i.test.predicate) |> fold (*) 1
        runNRounds nbRounds (fun r -> r % uint64 factors) monkeys

    let runWithRelief (monkeys: Monkey array) = snd (runAndEval (flip (/) 3UL) (Map.empty, monkeys))


let round1 inputs =
    let r = inputs |> Input.parse |> Game.runNRoundsWithRelief 20
    fst r
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.sortDescending
    |> Seq.take 2
    |> Seq.fold (*) 1UL

let round2 inputs =
    let r = inputs |> Input.parse |> Game.runNRoundsWithoutRelief 10000
    fst r
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.sortDescending
    |> Seq.take 2
    |> Seq.fold (*) 1UL

let private sample = __SOURCE_DIRECTORY__ + "/input.txt"

let round1Result = readRows sample |> round1

let round2Result = readRows sample |> round2