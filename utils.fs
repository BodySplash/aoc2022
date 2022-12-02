module AoC2022.utils

open System.IO
let readRows path =
    seq { yield! File.ReadLines(path) }