module AoC2022.Day7

open FSharpPlus
open System.Text.RegularExpressions
open AoC2022.utils

type FileSystemNode =
    | File of string * int
    | Directory of string * FileSystemNode list

type Command =
    | LS
    | CD of string

let private (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)

    if m.Success then
        Some(List.tail [ for g in m.Groups -> g.Value ])
    else
        None


module FileSystem =

    let private (|FileOutput|_|) i =
        match i with
        | Regex @"(\d+)\s(.*)" [ size; name ] -> Some(name, (int size))
        | _ -> None

    let private parseCommand (v: string) =
        if v.StartsWith "cd" then
            CD(v[3..])
        else
            LS

    let private (|Command|_|) (v: string) =
        if v.StartsWith "$" then
            Some(parseCommand v[2..])
        else
            None


    let parse inputs =

        let rec parse' current s' =
            match s' with
            | Command v :: tail ->
                match v with
                | CD ".." -> (current, tail)
                | CD dir ->
                    let newState, newTail =
                        parse' (Directory(dir, [])) tail

                    match current with
                    | Directory (name, nodes) -> parse' (Directory(name, newState :: nodes)) newTail
                    | _ -> (newState, tail)
                | _ -> parse' current tail
            | FileOutput (name, size) :: tail ->
                match current with
                | Directory (dirName, nodes) -> parse' (Directory(dirName, File(name, size) :: nodes)) tail
                | _ -> parse' current tail

            | _ :: tail -> parse' current tail
            | [] -> (current, [])

        match (parse' (Directory("", [])) inputs) |> fst with
        | Directory (_, head :: _) -> head
        | e -> e

    let private isFile =
        function
        | File _ -> true
        | _ -> false

    let private isDirectory =
        function
        | Directory _ -> true
        | _ -> false

    let fold f state node =
        let rec folder path s' node' =
            match node' with
            | File _ as current -> f s' path current
            | Directory (name, elems) as d ->
                let currPath = [ name ] |> List.append path
                let newState = f s' path d

                let fileStates =
                    elems
                    |> filter isFile
                    |> List.fold (folder currPath) newState

                elems
                |> filter isDirectory
                |> List.fold (folder currPath) fileStates

        folder [] state node

    let private concatPaths pathes = pathes |> String.concat "/"

    let appendToPath path name =
        (List.append path [ name ]) |> concatPaths



    let private fileSize =
        function
        | File (_, i) -> i
        | _ -> 0

    let dd (root: FileSystemNode) =

        let rec updatePaths acc paths value =
            match paths with
            | [] -> acc
            | _ ->
                let newAcc =
                    Map.change
                        (concatPaths paths)
                        (fun v ->
                            match v with
                            | Some c -> Some(c + value)
                            | _ -> None)
                        acc

                updatePaths newAcc (List.take ((List.length paths) - 1) paths) value

        let walker acc path f =
            match f with
            | Directory (s, _) ->
                let acc =
                    Map.add (appendToPath path s) 0 acc

                acc
            | File (_, i) -> updatePaths acc path i

        fold walker (Map []) root



let round1 =
    FileSystem.dd
    >> Map.toSeq
    >> map snd
    >> filter ((>=) 100000)
    >> sum


let private inputPath =
    __SOURCE_DIRECTORY__ + "/input.txt"

let inputs =
    readRows inputPath |> toList |> FileSystem.parse

let round1Result = inputs |> round1

let round2 node =
    let r = FileSystem.dd node
    let freeSpace = 70000000 - r["/"]
    let requirement = 30000000 - freeSpace

    r
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.sort
    |> Seq.tryFind (fun i -> i >= requirement)

let round2Result = inputs |> round2
