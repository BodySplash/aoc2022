module AoC2022.Day8

open FSharpPlus
open AoC2022.utils

type Tree = (int * int) * bool

type Direction =
    | TOP
    | RIGHT
    | BOTTOM
    | LEFT

module Input =
    let parse elems =
        let parseLine (elems': string) =
            elems'.ToCharArray() |> map (string >> int)

        elems |> Seq.map parseLine |> Seq.toArray

module Forest =

    let private walkTo dir (row, col) =
        match dir with
        | TOP -> (row - 1, col)
        | RIGHT -> (row, col + 1)
        | BOTTOM -> (row + 1, col)
        | LEFT -> (row, col - 1)

    let sizeAt forest (row, col) =
        Array.tryItem row forest |> Option.bind (Array.tryItem col)


    let rec isVisibleIn forest (row, col) size dir =
        let neighbour = walkTo dir (row, col)

        let p =
            sizeAt forest neighbour
            |> Option.fold
                (fun acc otherSize -> acc && size > otherSize && (isVisibleIn forest neighbour size dir))
                true

        p

    let rec countVisibleIn forest (row, col) size dir =
        let neighbour = walkTo dir (row, col)

        let p =
            sizeAt forest neighbour
            |> Option.fold
                (fun acc otherSize ->
                    let newAcc = acc + 1

                    if size <= otherSize then
                        newAcc
                    else
                        newAcc + (countVisibleIn forest neighbour size dir))
                0

        p

    let fold forest =
        let countVisibleIn = countVisibleIn forest

        let countVisibleInDirs pos size =
            let w = countVisibleIn pos size
            [ TOP; RIGHT; BOTTOM; LEFT ] |> Seq.map (fun d -> (d, w d))




        forest
        |> Array.indexed
        |> Array.fold
            (fun s (rowNumber, row) ->
                row
                |> Array.indexed
                |> Array.fold
                    (fun s' (colNumber, size) ->
                        ((rowNumber, colNumber), (countVisibleInDirs (rowNumber, colNumber) size)) :: s')
                    s)
            []


    let findVisibleTrees (forest: int array array) =

        let isVisibleIn = isVisibleIn forest

        let isVisibleInDirs pos size =
            let w = isVisibleIn pos size
            [ TOP; RIGHT; BOTTOM; LEFT ] |> Seq.map w |> Seq.fold (||) false

        let colLength col = Array.length forest[col]

        let isVisible (row, col) size =
            if
                row = 0
                || col = 0
                || row = (Array.length forest) - 1
                || col = (colLength col) - 1
            then
                true
            else
                isVisibleInDirs (row, col) size

        forest
        |> Array.indexed
        |> Array.fold
            (fun s (rowNumber, row) ->
                row
                |> Array.indexed
                |> Array.fold
                    (fun s' (colNumber, size) ->
                        ((rowNumber, colNumber), (isVisible (rowNumber, colNumber) size)) :: s')
                    s)
            []
        |> filter (snd >> (=) true)


let private inputPath = __SOURCE_DIRECTORY__ + "/input.txt"

let round1 i =
    i |> Input.parse |> Forest.findVisibleTrees |> Seq.length

let round1Result = readRows inputPath |> round1


let round2 i =
    i
    |> Input.parse
    |> Forest.fold
    |> List.map (fun (p, dirs) -> (p, dirs |> Seq.fold (fun acc (_, v) -> acc * v) 1))
    |> sortByDescending snd
    |> Seq.head

let round2Result = readRows inputPath |> round2 |> snd
