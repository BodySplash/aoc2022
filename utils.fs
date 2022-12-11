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
        
        
module Collection =
    /// Returns a sequence that yields chunks of length n.
    /// Each chunk is returned as a list.
    let split length (xs: seq<'T>) =
        let rec loop xs =
            [
                yield Seq.truncate length xs |> Seq.toList
                match Seq.length xs <= length with
                | false -> yield! loop (Seq.skip length xs)
                | true -> ()
            ]
        loop xs