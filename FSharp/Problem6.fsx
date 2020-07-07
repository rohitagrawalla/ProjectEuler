(*
The sum of the squares of the first ten natural numbers is, 385
The square of the sum of the first ten natural numbers is, 3025

Hence the difference between the sum of the squares of the first 
ten natural numbers and the square of the sum is 3025−385=2640

Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
*)

let sq x = x * x

let sumOfSquares xs = xs |> List.sumBy sq
let squareOfSum xs = xs |> List.sum |> sq

let solution = (sumOfSquares [1..100] - squareOfSum [1..100]) |> abs

printfn "Difference between the sum of the squares of the first one hundred natural numbers and the square of the sum is %A" solution

