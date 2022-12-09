module AoC2022.day9.Day9_test


open AoC2022.Day9
open Expecto

[<Tests>]
let tests =
    testList
        "Day 9"
        [

          testCase "Are neighbours"
          <| fun _ ->
              let r = Position.touching (4, 1) (3, 0)
              Expect.isTrue r ""


          testCase "Round 1 works on sample"
          <| fun _ ->
              let sample = [ "R 4"; "U 4"; "L 3"; "D 1"; "R 4"; "D 1"; "L 5"; "R 2" ]

              let result = sample |> round1

              Expect.equal result 13 "Count"

          testCase "Round 1" <| fun _ -> Expect.equal round1Result 5902 "Round 1"

          testCase "Round 2 works on sample"
          <| fun _ ->
              let sample =
                  [ "R 5"
                    "U 8"
                    "L 8"
                    "D 3"
                    "R 17"
                    "D 10"
                    "L 25"
                    "U 20"

                    ]

              let initial = ((0, 0), (Seq.replicate 9 (0, 0)) |> Seq.toList)


              let result = sample |> round initial

              Expect.equal result 36 "Count"
              
          testCase "Round 2" <| fun _ -> Expect.equal round2Result 2445 "Round 2"
              
              ]
