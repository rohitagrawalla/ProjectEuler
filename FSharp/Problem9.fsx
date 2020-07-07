(*
A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

a2 + b2 = c2
For example, 32 + 42 = 9 + 16 = 25 = 52.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.
*)

let isPythagoreanTriplet (a, b, c) = (a * a + b * b = c * c)

let isInOrder (a, b, c) = (a < b && b < c)

let generateTriplets =
    seq {
        for a in [1..1000] do
            for b in [a+1 .. 1000] do
                yield a, b, 1000 - (a + b)
    }

let solution = 
    generateTriplets
    |> Seq.filter isInOrder
    |> Seq.filter isPythagoreanTriplet
    |> Seq.head


printfn "The triplet is %A" solution