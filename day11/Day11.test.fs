module AoC2022.test.Day11

open Expecto
open AoC2022.Day11
open FSharpPlus


let sample =
    "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1"
        .Split "\n"

[<Tests>]
let tests =
    testList
        "Day 11"
        [ testCase "Parse"
          <| fun _ ->
              let result = Input.parse sample
              Expect.hasLength result 4 "Monkeys count"

              let monkey0 = result |> head

              Expect.equal monkey0.items [ 79UL; 98UL ] "Items"
              Expect.equal (monkey0.operation 10UL) 190UL "operation"
              Expect.equal monkey0.test.predicate 23 "predicate"
              Expect.equal monkey0.test.ifTrue 2 "true"
              Expect.equal monkey0.test.ifFalse 3 "false"

          testCase "Round"
          <| fun _ ->
              let r = sample |> Input.parse |> Game.runWithRelief

              Expect.hasLength r 4 ""
              Expect.containsAll r[0].items [ 20UL; 23UL; 27UL; 26UL ] "Monkey 0"
              Expect.containsAll r[1].items [ 2080UL; 25UL; 167UL; 207UL; 401UL; 1046UL ] "Monkey 1"
              Expect.containsAll r[2].items [] "Monkey 2"
              Expect.containsAll r[3].items [] "Monkey 3"

          testCase "Sample works on round 1"
          <| fun _ ->
              let sum = round1 sample

              Expect.equal sum 10605UL "Multiplied"

          testCase "Round 1" <| fun _ ->
              Expect.equal round1Result 102399UL "Round 1"
          
          testCase "Round 2 on sample after 20 turn" <| fun _ ->
              let r = fst (Game.runNRoundsWithoutRelief 20 (sample |> Input.parse))
              
              Expect.equal r[0] 99UL "Monkey 0"
              Expect.equal r[1] 97UL "Monkey 0"
              Expect.equal r[2] 8UL "Monkey 0"
              Expect.equal r[3] 103UL "Monkey 0"
          
          testCase "Sample works on round 2"
          <| fun _ ->
              let sum = round2 sample

              Expect.equal sum 2713310158UL "Multiplied"
          
          testCase "Round 2" <| fun _ ->
              Expect.equal round2Result 23641658401UL "Round 2"
          ]
