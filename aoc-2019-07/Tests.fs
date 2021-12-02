module Tests

open Swensen.Unquote
let  utest = test
open Expecto

open Solution
open Demoinput
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
    ]


