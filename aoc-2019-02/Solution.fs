module Solution

type Computer = {
    memory : Map<int, int>
    /// instruction pointer
    ip : int
    finished : bool }


let nextInstruction comp =
    [
        comp.memory.Item (comp.ip + 0)
        comp.memory.Item (comp.ip + 1)
        comp.memory.Item (comp.ip + 2)
        comp.memory.Item (comp.ip + 3)
    ]

let stop comp =
    { comp with
        finished = true
    }

let step comp =
    if comp.memory.Item comp.ip = 99 then stop comp else
    let binaryOp f a b target = 
        let av = comp.memory.Item a 
        let bv = comp.memory.Item b
        { comp with
            memory = comp.memory |> Map.add target (f av bv)
            ip = comp.ip + 4 }
    match nextInstruction comp with
    | [1;a;b;c] -> binaryOp (+) a b c 
    | [2;a;b;c] -> binaryOp (*) a b c 
    | x -> failwithf "unexpected operation: %A" x

let parse (input:string) =
    {
        ip = 0
        memory =
            input.Split ','
            |> Seq.map int
            |> Seq.indexed
            |> Map
        finished = false
    }

let run comp = 
    let rec loop c =
        if c.finished then c else
        c |> step |> loop
    loop comp

let genericSolve noun verb input =
    let comp = parse input 
    { comp with
        memory =
            comp.memory
            |> Map.add 1 noun
            |> Map.add 2 verb
    }
    |> run
    |> fun c -> c.memory.Item 0

let solve = genericSolve 12 2

let solve2 input =
    [   for noun in 0..99 do
        for verb in 0..99 do
            if genericSolve noun verb input = 19690720
            then yield 100*noun + verb ]
    |> List.head
