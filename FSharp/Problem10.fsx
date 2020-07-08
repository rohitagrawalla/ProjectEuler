(*
The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.
*)

#load "./Utils.fsx"

open Utils.Math

let twoMillion = 2L * 1000L * 1000L

#time

let solution =
    primesNew
    |> Seq.map int64
    |> Seq.takeWhile (fun x -> x <= twoMillion)
    |> Seq.sum

printfn "Sum of primes under two million is %A" solution