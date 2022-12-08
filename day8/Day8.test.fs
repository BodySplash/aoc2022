module AoC2022.day8.Day8_test

open AoC2022.Day8
open Expecto

let sample = [ "30373"; "25512"; "65332"; "33549"; "35390" ]

[<Tests>]
let tests =
    testList
        "Day 8"
        [

          testCase "Parse"
          <| fun _ -> Expect.equal (Input.parse [ "12"; "34" ]) [| [| 1; 2 |]; [| 3; 4 |] |] ""

          testCase "Say visible on sample"
          <| fun _ ->
              let r = sample |> round1
              
              Expect.equal r 21 ""
              
          testCase "Round 1" <| fun _ ->
              Expect.equal round1Result 1546 "ok"

          
          testCase "poc round 2" <| fun _ ->
              
              let forest = sample |> Input.parse
              
              let t = Forest.countVisibleIn forest (3, 2) 5 Direction.TOP
              let r = Forest.countVisibleIn forest (3, 2) 5 Direction.RIGHT
              let b = Forest.countVisibleIn forest (3, 2) 5 Direction.BOTTOM
              let l = Forest.countVisibleIn forest (3, 2) 5 Direction.LEFT
              
              Expect.equal t 2 "top"
              Expect.equal l 2  "left"
              Expect.equal b 1 "bottom"
              Expect.equal r 2 "right"
              
          testCase "Round 2 on sample" <| fun _ ->
              let r = sample |> round2
              
              Expect.equal r ((3, 2), 8) ""
              
          testCase "Round 2" <| fun _ ->
              Expect.equal round2Result 519064 "Round 2"
          ]
