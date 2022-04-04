module Solution
open System

let rec parse w h (s:string) = [
        if s = "" then () else
        yield
            s.Substring(0, w*h).ToCharArray()
            |> Array.chunkBySize w
            |> Array.map String
            |> Array.toList
        yield! parse w h (s.Substring(w*h))
    ]


let countDigits c =
    String.concat ""
    >> Seq.filter ((=)c)
    >> Seq.length

let eval layers =
    let layer = layers |> List.minBy (countDigits '0')
    countDigits '1' layer * countDigits '2' layer

let solve (input:string) =
    input
    |> parse 25 6
    |> eval

let overlay (a:string, b:string) =
    Array.zip (a.ToCharArray()) (b.ToCharArray())
    |> Array.map (function
        | '2', c -> c
        |  c,  _ -> c)
    |> String

let renderTwo l1 l2 =
    List.zip l1 l2
    |> List.map overlay

let render = List.reduce renderTwo 


let solve2 (input:string) =
    let s =
        input
        |> parse 25 6
        |> render
        |> String.concat "\r\n"
    "\r\n" + s.Replace("0", "  ").Replace("1", "11")

