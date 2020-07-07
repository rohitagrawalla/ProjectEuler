(*
The prime factors of 13195 are 5, 7, 13 and 29.
What is the largest prime factor of the number 600851475143 ?
*)
#load "./Utils.fsx"


open Utils


let largestPrimeFactor n = n |> primeFactors |> List.max

printfn "Largest prime factor of the number 600851475143 is %A" (largestPrimeFactor 600851475143L)
