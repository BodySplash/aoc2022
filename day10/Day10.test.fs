module AoC2022.day10.Day10_test

open AoC2022.Day10
open AoC2022.utils
open Expecto


let smallSample = [ "noop"; "addx 3"; "addx -5" ]

let private sample = __SOURCE_DIRECTORY__ + "/sample.txt"
let bigSample = readRows sample

[<Tests>]
let tests =
    testList
        "Day 10"
        [

          testCase "parse negative addx"
          <| fun _ ->
              let r = Command.parse "addx -20"

              Expect.equal r (AddX -20) ""

          testCase "process small example"
          <| fun _ ->
              let cycles = smallSample |> Command.parseAll |> CPU.toCycles

              Expect.equal (cycles |> Seq.toList) [ 1; 1; 1; 4; 4; -1 ] ""

          testCase "round 1 works on  sample"
          <| fun _ ->
              let cycles = bigSample |> Command.parseAll |> CPU.toCycles

              Expect.equal (Signal.strength 20 cycles) 420 ""
              Expect.equal (Signal.strength 60 cycles) 1140 ""
              Expect.equal (Signal.strength 100 cycles) 1800 ""
              Expect.equal (Signal.strength 140 cycles) 2940 ""
              Expect.equal (Signal.strength 180 cycles) 2880 ""
              Expect.equal (Signal.strength 220 cycles) 3960 ""
              Expect.equal (Signal.sum [ 20; 60; 100; 140; 180; 220 ] cycles) 13140 ""

          testCase "round 1" <| fun _ -> Expect.equal round1Result 14040 ""
          
          testCase "Draw sample" <| fun _ ->
                let screen = bigSample |> Command.parseAll |> CPU.toCycles |> CRT.draw
                
                Expect.equal screen "##..##..##..##..##..##..##..##..##..##..\n\
                                     ###...###...###...###...###...###...###.\n\
                                     ####....####....####....####....####....\n\
                                     #####.....#####.....#####.....#####.....\n\
                                     ######......######......######......####\n\
                                     #######.......#######.......#######.....\n\
                                     ." "Sample"
          testCase "Draw real" <| fun _ ->
                Expect.equal round2Result "####..##...##....##.####...##.####.#....\n\
                                           ...#.#..#.#..#....#....#....#.#....#....\n\
                                           ..#..#....#.......#...#.....#.###..#....\n\
                                           .#...#.##.#.......#..#......#.#....#....\n\
                                           #....#..#.#..#.#..#.#....#..#.#....#....\n\
                                           ####..###..##...##..####..##..#....####.\n\
                                           ." "Round 2"
          ]
