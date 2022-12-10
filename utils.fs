[<AutoOpen>]
module AoC2022.utils

open System.IO
open System.Text.RegularExpressions

let readRows path = seq { yield! File.ReadLines(path) }

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)

    if m.Success then
        Some(List.tail [ for g in m.Groups -> g.Value ])
    else
        None