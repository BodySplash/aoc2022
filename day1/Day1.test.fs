module Day1_test

open Expecto
open AoC2022.Day1

[<Tests>]
let tests =
    testList "Day 1" [
        testCase "Round 1" <| fun _ ->
            Expect.equal round1 70374u "fake"
            
        testCase "Round 2" <| fun _ ->
            Expect.equal round2 204610u "fake"
    ]