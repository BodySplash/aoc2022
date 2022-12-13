module AoC2022.test.Day12

open Expecto
open AoC2022.Day12

let sample =
    "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi"
        .Split("\n")

[<Tests>]
let tests =
    testList
        "Day 12"
        [

          testCase "Parse sample"
          <| fun _ ->
              let plan, map = Input.parse sample

              Expect.equal (map.GetUpperBound 0) 4 ""
              Expect.equal (map.GetUpperBound 1) 7 ""
              Expect.equal plan.startPos (0, 0) "start"
              Expect.equal plan.endPos (2, 5) "end"
              Expect.equal (map[0, 0]) 'a' ""
              Expect.equal (map[2, 5]) 'z' ""

          testCase "Find routes"
          <| fun _ ->
              let r = sample |> Input.parse ||> PathFinder.findShortest

              Expect.isSome r "found"

          testCase "Sample works for round 1"
          <| fun _ ->
              let r = sample |> round1

              Expect.equal r (Some 31) "Round 1 sample"

          testCase "Round 1"
          <| fun _ ->
              let r = inputs |> round1

              Expect.equal r (Some 330) "Round 1"
              
          test "Find all a" {
              let world = sample |> Input.parse |> snd
              let positions = World.allPosWithValue world 'a'
              Expect.hasLength positions 6 ""
          }
          
          test "Sample works on round 2" {
              let r = sample |> round2
              
              Expect.equal r (Some 29) "Shortest"
          }
          
          ptest "Round 2" {
              let r = inputs |> round2
              
              Expect.equal r (Some 321) "Round 2"
          }
          
          ]
