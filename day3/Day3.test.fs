module AoC2022.day3.Day3_test

open Expecto
open AoC2022.Day3

[<Tests>]
let tests =
    testList
        "Day 3"
        [

          testCase "Maps priorities"
          <| fun _ ->
              Expect.equal priorities['a'] 1 "a"
              Expect.equal priorities['A'] 27 "A"
              Expect.equal priorities['Z'] 52 "Z"

          testCase "Parse one ruck sack"
          <| fun _ ->
              let actual = splitRuckSack "hDsD"

              Expect.hasLength actual 2 "Size"
              Expect.containsAll actual [ [| 'h'; 'D' |]; [| 's'; 'D' |] ] ""

          testCase "Find common elements"
          <| fun _ ->
              let actual =
                  findSharedItems [ [| 'h'; 'D' |]
                                    [| 's'; 'D' |] ]

              Expect.contains actual 'D' "Ok"

          testCase "Find common elements only once"
          <| fun _ ->
              let actual =
                  findSharedItems [ [| 'h'; 'h' |]
                                    [| 'h'; 'D' |] ]

              Expect.hasLength actual 1 "1 length"
              Expect.contains actual 'h' "Ok"

          testCase "Works for first example"
          <| fun _ ->
              let actual =
                  [ "vJrwpWtwJgWrhcsFMMfFFhFp" ] |> round1


              Expect.equal actual 16 "First exemple"

          testCase "Works for full example"
          <| fun _ ->
              let seed =
                  [ "vJrwpWtwJgWrhcsFMMfFFhFp"
                    "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
                    "PmmdzqPrVvPwwTWBwg"
                    "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
                    "ttgJtRGJQctTZtZT"
                    "CrZsJsPPZsGzwwsLwLmpwMDw" ]

              let actual = seed |> round1

              Expect.equal actual 157 "First example"

          testCase "Round 1"
          <| fun _ -> Expect.equal round1Result 7821 "fake"

          testCase "Round 2 first example"
          <| fun _ ->
              let seed =
                  [ "vJrwpWtwJgWrhcsFMMfFFhFp"
                    "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
                    "PmmdzqPrVvPwwTWBwg" ]

              Expect.equal (round2 seed) (Some 18) "plop"

          testCase "Round 2 second example"
          <| fun _ ->
              let seed =
                  [ "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
                    "ttgJtRGJQctTZtZT"
                    "CrZsJsPPZsGzwwsLwLmpwMDw" ]

              Expect.equal (round2 seed) (Some 52) "plop"
          testCase "Round 2 result"
          <| fun _ -> Expect.equal round2Result (Some 2752) "Real"

          ]
