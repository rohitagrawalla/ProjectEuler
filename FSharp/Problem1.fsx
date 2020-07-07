(*
If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
Find the sum of all the multiples of 3 or 5 below 1000.
*)
#load "./Utils.fsx"


open Utils

let sumOfMultiplesOf3And5Under1000 =
    [ 1 .. 999 ]
    |> Seq.filter (isAnyFactorOf [ 3; 5 ])
    |> Seq.sum

printfn "Sum of all the multiples of 3 or 5 below 1000 is %A" sumOfMultiplesOf3And5Under1000
