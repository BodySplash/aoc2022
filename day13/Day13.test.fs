module AoC2022.test.Day13


open AoC2022.Day13
open Expecto

let sample =
    "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]"
        .Split()



[<Tests>]
let tests =
    testList
        "Day 13"
        [ test "Parse simple value" {
              let r = Input.parsePair [ "[]"; "[1,20,[3,4]]" ]

              Expect.equal r [ (List [], List [ Value 1; Value 20; List [ Value 3; Value 4 ] ]) ] ""
          }
          test "Sample works on round 1" {
              let r = round1 sample

              Expect.equal r 13 "Sample"
          }

          test "Round 1" {
              let r = round1 inputs

              Expect.equal r 5882 "Round 1"
          }
          
          test "Round 2 works on sample" {
              let r = round2 sample
              
              Expect.equal r 140 "Sample"
          }
          
          test "Round 2" {
              let r = round2 inputs
              
              Expect.equal r 24948 "Round 2"
          }

          ]
