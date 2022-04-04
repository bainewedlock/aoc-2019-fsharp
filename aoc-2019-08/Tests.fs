module Tests

open Swensen.Unquote
let  utest = test
open Expecto

open Solution
open Demoinput

[<Tests>]
let all =
    testList "all" [
        test "parsing" {
            let layers = parse 3 2 "123456789012"
            layers.Length =! 2
            layers.[0] =! [
                "123"
                "456" ]
        }
        test "counting digits" {
            let layer = [ "110" ; "201" ]
            countDigits '1' layer =! 3
            countDigits '0' layer =! 2
            countDigits '3' layer =! 0
        }
        test "eval" {
            eval [["111";"022"]; ["110"; "002"]] =! 6
        }
        test "rendering" {
            render [["02";"22"]; ["11";"22"]; ["22";"12"]; ["00"; "00"]] =!
            ["01"; "10"]
        }
        test "overlay" {
            overlay ("02","01") =! "01"
        }
    ]


