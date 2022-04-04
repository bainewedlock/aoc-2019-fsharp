open Solution

let input = System.IO.File.ReadAllText "input.txt"

[<EntryPoint>]
let main argv =

    solve input
    |> printfn "solution 1: %A"
    printfn "(accepted answer: 1742)"

    solve2 input
    |> printfn "solution 2: %A"
    printfn "(accepted answer: GJYEA)"

    0
