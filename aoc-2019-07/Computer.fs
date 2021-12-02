namespace Computer

type Computer = {
    memory : Map<int, int>
    /// instruction pointer
    ip : int
    inputs : int list
    output : int option
    finished : bool }

module Computer =

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

    let setInputs a b comp =
        { comp with inputs = [a;b] }

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
                 else { comp with ip = comp.ip + 3 }
        match nextInstruction comp with
        | 99, _ ->
            { comp with finished = true }
        | 1, [a;b;Position p] ->
            writeAndMove p (read a + read b) 4
        | 2, [a;b;Position p] ->
            writeAndMove p (read a * read b) 4
        | 3, [Position p;_;_] ->
            if comp.inputs.IsEmpty 
            then failwithf "could not read from input (anymore)" else
            { writeAndMove p comp.inputs.Head 2
                with inputs = comp.inputs.Tail }
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
            inputs = [0]
            output = None
            memory =
                input.Split ','
                |> Seq.map int
                |> Seq.indexed
                |> Map
            finished = false
        }

    let isWaitingForInput = nextInstruction >> fst >> (=)3

    let run comp = 
        let comp = { comp with output = None }
        let rec loop c =
            if c.finished
                || (isWaitingForInput c && c.inputs = [])
                || (c.output.IsSome)
            then c
            else c |> step |> loop
        loop comp
