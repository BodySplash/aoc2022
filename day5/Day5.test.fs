module AoC2022.day5.Day5_test

open Expecto
open AoC2022.Day5

let sample =
    [ "    [D]    "
      "[N] [C]    "
      "[Z] [M] [P]"
      " 1   2   3"
      ""
      "move 1 from 2 to 1"
      "move 3 from 1 to 3"
      "move 2 from 2 to 1"
      "move 1 from 1 to 2" ]

[<Tests>]
let tests =
    testList
        "day 5"
        [ testCase "Parse sample"
          <| fun _ ->
              let (stacks, instructions) =
                  Inputs.parse sample

              Expect.equal
                  stacks
                  [ [ 'N'; 'Z' ]
                    [ 'D'; 'C'; 'M' ]
                    [ 'P' ] ]
                  "stacks"

              Expect.hasLength instructions 4 "Instructions count"

              Expect.sequenceContainsOrder
                  instructions
                  [ { Number = 1; From = 2; To = 1 }
                    { Number = 3; From = 1; To = 3 }
                    { Number = 2; From = 2; To = 1 }
                    { Number = 1; From = 1; To = 2 } ]
                  "Instructions"
          testCase "Applies move"
          <| fun _ ->
              let stakes =
                  [ [ 'N'; 'Z' ]; [ 'D'; 'C'; 'M' ] ]

              let newStakes =
                  Crane.applyMove stakes { From = 1; To = 2; Number = 1 }

              Expect.equal newStakes [ [ 'Z' ]; [ 'N'; 'D'; 'C'; 'M' ] ] ""
          testCase "Sample moves work"
          <| fun _ ->
              let (stacks, instructions) =
                  Inputs.parse sample

              let result =
                  Crane.applyMoves stacks instructions

              Expect.equal
                  result
                  [ [ 'C' ]
                    [ 'M' ]
                    [ 'Z'; 'N'; 'D'; 'P' ] ]
                  "Sample"

          testCase "Round 1 works on sample"
          <| fun _ ->
              let result = round1 (Inputs.parse sample)

              Expect.equal result "CMZ" "Ok"
              
          testCase "Round 1" <| fun _ ->
              Expect.equal round1Result "FJSRQCFTN" "Real"

          testCase "Updated move logic works on sample" <| fun _ ->
              let result = round2 (Inputs.parse sample)
              
              Expect.equal result "MCD" "Sample round2"
              
          testCase "Round 2 result" <| fun _ ->
              Expect.equal round2Result "CJVLJQPHS" "Real"
          ]
