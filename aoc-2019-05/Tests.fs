module Tests

open Swensen.Unquote
let  utest = test
open Expecto

open Solution
open Demoinput

[<Tests>]
let all =
    testList "all" [
        testList "part 1" [
            test "3,a: store input at address" {
                let c = {
                    ip = 0
                    input = 123
                    output = None
                    finished = false
                    memory = Map [
                        0,3
                        1,10
                        4,99 ] }
                let state2 = step c
                state2.memory.Item 10 =! 123
                state2.ip    =! 2 }
            test "4,a: store address value to output" {
                let c = {
                    ip = 0
                    input = 0
                    output = None
                    finished = false
                    memory = Map [
                        0,4
                        1,10
                        4,99
                        10,50 ] }
                let state2 = step c
                state2.output =! Some 50
                state2.ip     =! 2 }
        ]
        testList "part 2" [
            test "parameters" {
            parameters [1102;3;4;5] =! [
                    Immediate 3
                    Immediate 4
                    Position 5 ] }
            test "demoinput runs through" { solve demoinput =! None }
        ]
    ]


