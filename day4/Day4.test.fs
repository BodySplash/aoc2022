module AoC2022.day4.Day4_test


open AoC2022.Day4
open Expecto

let sample =
    [ "2-4,6-8"
      "2-3,4-5"
      "5-7,7-9"
      "2-8,3-7"
      "6-6,4-6"
      "2-6,4-8" ]

[<Tests>]
let tests =
    testList
        "Day 4"
        [ testCase "parse line"
          <| fun _ ->
              let line = "19-71,6-18"

              let pairs = Input.parseLine line
              Expect.equal pairs ((19, 71), (6, 18)) ""

          testCase "tells if one section includes other"
          <| fun _ ->
              Expect.isTrue (Sections.anyContains ((1, 5), (2, 4))) ""
              Expect.isTrue (Sections.anyContains ((2, 4), (1, 5))) ""
              Expect.isFalse (Sections.anyContains ((1, 2), (3, 4))) ""

          testCase "round 1 works for example"
          <| fun _ ->
              let result = round1 sample

              Expect.equal result 2 "Sample"

          testCase "round 1 result"
          <| fun _ -> Expect.equal round1Result 448 "Real"

          testCase "says overlap"
          <| fun _ ->
              Expect.isTrue (Sections.overlaps ((3, 5), (2, 4))) ""
              Expect.isTrue (Sections.overlaps ((5, 7), (7, 9))) ""
              Expect.isTrue (Sections.overlaps ((6, 6), (6, 7))) ""
              Expect.isFalse (Sections.overlaps ((2, 3), (4, 5))) ""

          testCase "round 2 works for example"
          <| fun _ -> Expect.equal (round2 sample) 4 "Sample"

          testCase "round 2 result"
          <| fun _ -> Expect.equal round2Result 794 "Real"
          
          ]
