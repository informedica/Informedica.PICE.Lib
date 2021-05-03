module Tests

open System
open Expecto

[<Tests>]
let tests =
    testList "samples" [
        test "passing test" {
            Expect.isTrue true "is true"
        }
     ]
