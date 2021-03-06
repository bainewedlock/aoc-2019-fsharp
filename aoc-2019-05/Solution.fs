module Solution

type Computer = {
    memory : Map<int, int>
    /// instruction pointer
    ip : int
    input : int
    output : int option
    finished : bool }

type Parameter = Immediate of value:int | Position of address:int

type Instruction = {
    opcode : int
    parameters : Parameter List }

let parameters (xs:int list) =
    (string xs.Head).PadLeft(5, '0').Substring(0, 3)
    |> Seq.toList
    |> List.rev
    |> List.indexed
    |> List.map (fun (i,c) -> 
        if c = '0'
        then Position xs.[i+1]
        else Immediate xs.[i+1] )

let nextInstruction comp =
    let numbers =
        [0..3]
        |> List.map ((+)comp.ip)
        |> List.map (comp.memory.TryFind >> Option.defaultValue 0)
    numbers.Head % 100, (parameters numbers)

let step comp =
    let read = function
        | Immediate v -> v
        | Position  p -> comp.memory.Item p
    let writeAndMove addr value move = 
        { comp with
            memory = comp.memory.Add(addr, value)
            ip = comp.ip + move }
    let jumpIf b newIp =
        if b then { comp with ip = newIp }
             else { comp with ip = comp.ip + 2 }
    match nextInstruction comp with
    | 99, _ ->
        { comp with finished = true }
    | 1, [a;b;Position p] ->
        writeAndMove p (read a + read b) 4
    | 2, [a;b;Position p] ->
        writeAndMove p (read a * read b) 4
    | 3, [Position p;_;_] ->
        writeAndMove p comp.input 2
    | 4, [a;_;_] ->
        { comp with output = Some (read a); ip = comp.ip + 2 }
    | 5, [a;target;_] ->
        jumpIf (read a <> 0) (read target)
    | 6, [a;target;_] ->
        jumpIf (read a =  0) (read target)
    | 7, [a;b;Position target] ->
        writeAndMove target (if read a < read b then 1 else 0) 4
    | 8, [a;b;Position target] ->
        writeAndMove target (if read a = read b then 1 else 0) 4
    | x -> failwithf "unexpected instruction: %A" x

let parse (input:string) =
    {
        ip = 0
        input = 1
        output = None
        memory =
            input.Split ','
            |> Seq.map int
            |> Seq.indexed
            |> Map
        finished = false
    }

let run comp = 
    let rec loop c =
        if c.finished
        then c
        else c |> step |> loop
    loop comp

let solveGeneric inputValue puzzleInput =
    (run { parse puzzleInput with input = inputValue }).output

let solve  = solveGeneric 1
let solve2 = solveGeneric 5
