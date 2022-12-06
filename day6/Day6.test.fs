module AoC2022.day6.Day6_test

open Expecto
open AoC2022.Day6

let sample0 =
    "mjqjpqmgbljsphdztnvjfqwrcgsmlb"

let sample1 = "bvwbjplbgvbhsrlpgdmjqwftvncz"

let sample2 = "nppdvjthqldpwncqszvftbrmjlhg"

let sample3 =
    "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"

let sample4 =
    "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"

[<Tests>]
let tests =
    testList
        "Day 6"
        [ testCase "Round 1 works on samples"
          <| fun _ ->
              Expect.equal (findStartPacket sample0) 7 "sample"
              Expect.equal (findStartPacket sample1) 5 "First sample"
              Expect.equal (findStartPacket sample2) 6 "Second sample"
              Expect.equal (findStartPacket sample3) 10 "Third sample"
              Expect.equal (findStartPacket sample4) 11 "Fourth sample"

          testCase "Round 1 result"
          <| fun _ -> Expect.equal round1Result (Some 1702) "Round 1"

          testCase "Round 2 works on samples"
          <| fun _ ->
              Expect.equal (findStartMessage sample0) 19 "sample"
              Expect.equal (findStartMessage sample1) 23 "First sample"
              Expect.equal (findStartMessage sample2) 23 "Second sample"
              Expect.equal (findStartMessage sample3) 29 "Third sample"
              Expect.equal (findStartMessage sample4) 26 "Fourth sample"

          testCase "Round 2 result"
          <| fun _ -> Expect.equal round2Result (Some 3559) "Round 2" ]
