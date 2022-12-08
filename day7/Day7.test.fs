module AoC2022.day7.Day7_test

open Expecto
open AoC2022.Day7

let sample =
    [ "$ cd /"
      "$ ls"
      "dir a"
      "14848514 b.txt"
      "8504156 c.dat"
      "dir d"
      "$ cd a"
      "$ ls"
      "dir e"
      "29116 f"
      "2557 g"
      "62596 h.lst"
      "$ cd e"
      "$ ls"
      "584 i"
      "$ cd .."
      "$ cd .."
      "$ cd d"
      "$ ls"
      "4060174 j"
      "8033020 d.log"
      "5626152 d.ext"
      "7214296 k" ]

[<Tests>]
let tests =
    testSequenced
    <| testList
        "Day 7"
        [

          testCase "simple parsing"
          <| fun _ ->
              let r =
                  FileSystem.parse [ "$ cd /"
                                     "$ cd a"
                                     "4060174 j"
                                     "$ cd .."
                                     "2 b" ]

              Expect.equal
                  r
                  (Directory(
                      "/",
                      [ File("b", 2)
                        Directory("a", [ File("j", 4060174) ]) ]
                  ))
                  ""
          testCase "full sample parsing"
          <| fun _ ->
              let r = FileSystem.parse sample

              Expect.equal
                  r
                  (Directory(
                      "/",
                      [ Directory(
                            "d",
                            [ File("k", 7214296)
                              File("d.ext", 5626152)
                              File("d.log", 8033020)
                              File("j", 4060174) ]
                        )
                        Directory(
                            "a",
                            [ Directory("e", [ File("i", 584) ])
                              File("h.lst", 62596)
                              File("g", 2557)
                              File("f", 29116) ]
                        )
                        File("c.dat", 8504156)
                        File("b.txt", 14848514) ]
                  ))
                  ""


          testCase "Round 1 works on sample"
          <| fun _ ->
              let r = FileSystem.parse sample |> round1

              Expect.equal r 95437 ""

          testCase "Round 1"
          <| fun _ -> Expect.equal round1Result 1315285 "Ok"

          testCase "Round 2 works on sample"
          <| fun _ ->
              let r = FileSystem.parse sample |> round2

              Expect.equal r (Some 24933642) ""
          testCase "Round 2 result"
          <| fun _ -> Expect.equal round2Result (Some 9847279) "ok" ]
