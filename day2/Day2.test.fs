module AoC2022.day2.Day2_test

open Expecto
open AoC2022.Day2
[<Tests>]
let tests =
    testList
        "Day 2"
        [
            
            testCase "Parse one round" <| fun _ ->
                let r = Round1.parseTurn "A Y"
                
                Expect.equal r (Some (Turn (Figure.Rock, Figure.Paper))) ""
            
            testCase "Parse many" <| fun _ ->
                let r = Round1.parseTurns ["A Y"; "B Z"]
                
                Expect.equal (Some [Turn(Figure.Rock, Figure.Paper);Turn(Figure.Paper, Figure.Scissors)]) r ""
                
            testCase "Should score" <| fun _ ->
                let score1 = Score.fromTurn (Turn(Figure.Rock, Figure.Paper))
                let score2 = Score.fromTurn (Turn(Figure.Paper, Figure.Rock))
                let score3 = Score.fromTurn (Turn(Figure.Scissors, Figure.Scissors))
                let score4 = Score.fromTurn (Turn(Figure.Scissors, Figure.Paper))
                let score5 = Score.fromTurn (Turn(Figure.Scissors, Figure.Rock))
                
                Expect.equal score1 8 "score1"
                Expect.equal score2 1 "score2"
                Expect.equal score3 6 "score3"
                Expect.equal score4 2 "score4"
                Expect.equal score5 7 "score5"
                
            testCase "Round 1" <| fun _ ->
                //Expect.hasLength inputs 2500 ""
                Expect.equal round1 (Some 10718) "Fake"
                
            testCase "Round 2" <| fun _ ->
                Expect.equal round2 (Some 14652) "Fake"
        ]
