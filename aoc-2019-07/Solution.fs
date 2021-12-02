module Solution

open Computer

// Source: http://www.fssnip.net/4u/title/Very-Fast-Permutations
let rec permutations = function
    | []    -> seq [[]]
    | x::xs -> Seq.collect (insertions x) (permutations xs)
and insertions x = function
    | []            -> [[x]]
    | (y::ys) as xs ->
        (x::xs)::(List.map (fun x -> y::x) (insertions x ys))

let connect comp prev phase =
    comp
    |> Computer.setInputs phase prev.output.Value
    |> Computer.run

let tryPermutation comp phases =
    phases
    |> Seq.fold (connect comp) { comp with output = Some 0 }
    |> fun c -> c.output

let tryPermutation2 comp phases =
    let rec loop (prevOut,computers) =
        let last = List.last computers
        if last.finished then prevOut else
        computers
        |> List.indexed
        |> List.fold (fun (prevOut2,cs) (i,c) ->
            let c2 =
                { c with inputs = c.inputs @ [prevOut2] }
                |> Computer.run
            let out =
                if c2.finished then prevOut2 else c2.output.Value
            (out, cs @ [c2])) (prevOut, [])
        |> loop 
    let computers =
        phases |> List.map (fun p -> { comp with inputs = [p] })
    loop (0,computers)


let solve input =
    [0..4]
    |> permutations
    |> Seq.map (tryPermutation (Computer.parse input))
    |> Seq.max

let solve2 input =
    [5..9]
    |> permutations
    |> Seq.map (tryPermutation2 (Computer.parse input))
    |> Seq.max

let debug () =
    let comp =
        "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"
        |> Computer.parse
    let permutation = [9;8;7;6;5]
    tryPermutation2 comp permutation
    |> printfn "%A"

