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
            test "1,a,b,c: addition" {
                let c = {
                    ip = 0
                    finished = false
                    memory = Map [
                        0,1
                        1,10
                        2,20
                        3,30
                        4,99
                        10,5
                        20,7 ] }
                let state2 = step c
                state2.memory.Item 30 =! 12
                state2.ip    =! 4 }
            test "1,a,b,c: multiplication" {
                let c = {
                    ip = 0
                    finished = false
                    memory = Map [
                        0,2
                        1,10
                        2,20
                        3,30
                        4,99
                        10,5
                        20,7 ] }
                let state2 = step c
                state2.ip =! 4
                state2.memory.Item 30 =! 35 }
            test "99: exit" {
                let comp = {
                    ip = 12 
                    finished = false
                    memory = Map [12, 99; 13,0; 14,0] }
                let comp2 = step comp
                comp2.finished =! true }
            test "parse" {
                let comp = parse "1,9,10,3,2,3,11,0,99,30,40,50"
                comp.ip =! 0
                comp.memory =! Map [
                    0,1
                    1,9
                    2,10
                    3,3
                    4,2
                    5,3
                    6,11
                    7,0
                    8,99
                    9,30
                    10,40
                    11,50 ] }
            test "run input" {
                let comp = "1,9,10,3,2,3,11,0,99,30,40,50" |> parse |> run
                comp.memory.Item 0 =! 3500 }
            test "example program #3" {
                let comp = "1,1,1,4,99,5,6,0,99" |> parse |> run
                comp.memory.Item 0 =! 30
                comp.memory.Item 4 =! 2 }
            test "example program #4" {
                let comp = "2,4,4,5,99,0" |> parse |> run
                comp.memory.Item 5 =! 9801 }
        ]
    ]


