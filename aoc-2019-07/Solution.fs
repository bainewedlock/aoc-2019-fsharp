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

let solve input =
    [0..4]
    |> permutations
    |> Seq.map (tryPermutation (Computer.parse input))
    |> Seq.max

let solve2 input =
    let c = Computer.parse input
    c
