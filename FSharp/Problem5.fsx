(*
2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
*)
#load "./Utils.fsx"

open Utils.Math

printfn "Smallest positive number that is evenly divisible by all of the numbers from 1 to 20 is %A"
    ([1..20] |> lcmMany)
