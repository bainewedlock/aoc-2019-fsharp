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

let folder comp prev phase =
    comp
    |> Computer.setInputs phase prev.output.Value
    |> Computer.run

let tryPermutation comp phases =
    phases
    |> Seq.fold (folder comp) { comp with output = Some 0 }
    |> fun c -> c.output

let folder2 (prevOut, cs) comp =
    let comp2 = Computer.addInput comp prevOut |> Computer.run
    let out =
        if comp2.finished
        then prevOut
        else comp2.output.Value
    out, cs @ [comp2]

let allFinished = List.forall Computer.isFinished

let tryPermutation2 comp phases =
    let rec loop (out, cs) =
        if allFinished cs then out else
        cs
        |> List.fold folder2 (out, [])
        |> loop 
    let initialInput = 0
    let computers = phases |> List.map (Computer.addInput comp)
    loop (initialInput, computers)

let genericSolve phases fTry input =
    phases
    |> permutations
    |> Seq.map (fTry (Computer.parse input))
    |> Seq.max

let solve  = genericSolve [0..4] tryPermutation
let solve2 = genericSolve [5..9] tryPermutation2
