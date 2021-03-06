(*
Each new term in the Fibonacci sequence is generated by adding the previous two terms.
By starting with 1 and 2, the first 10 terms will be:

1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...

By considering the terms in the Fibonacci sequence whose values do not exceed four million,
find the sum of the even-valued terms.
*)

let fibSeq =
    Seq.unfold (fun (a, b) -> Some(a + b, (b, a + b))) (1, 2)

let isEven x = x % 2 = 0

let solution =
    fibSeq
    |> Seq.filter isEven
    |> Seq.takeWhile (fun x -> x <= 4000000)
    |> Seq.sum


printfn "Sum of even valued fib numbers not exceeding four million is %A" solution
