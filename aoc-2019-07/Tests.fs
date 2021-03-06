module Tests

open Swensen.Unquote
let  utest = test
open Expecto

open Solution
open Computer

[<Tests>]
let all =
    testList "all" [
        testList "part 1" [
            test "permutations" {
                let result = permutations [1;2;3]
                result |> Seq.length =! 6
            }
            test "multiple inputs" {
                let comp =
                    "3,31,3,32,99"
                    |> Computer.parse 
                    |> Computer.setInputs 10 20
                let result = Computer.run comp
                result.memory.Item 31 =! 10
                result.memory.Item 32 =! 20
            }
            test "try example #3" {
                let comp =
                    "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33, 1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"
                    |> Computer.parse
                let permutation = [1;0;4;3;2]
                tryPermutation comp permutation =! Some 65210
            }
            test "solve example #3"{
                solve "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33, 1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"
                =! Some 65210
            }
        ]
        testList "part 2" [
            test "try example #1" {
                let comp =
                    "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"
                    |> Computer.parse
                let permutation = [9;8;7;6;5]
                tryPermutation2 comp permutation =! 139629729 }
        ]
    ]


