(*
A palindromic number reads the same both ways. The largest palindrome made from the product of
two 2-digit numbers is 9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit numbers.
*)
#load "./Utils.fsx"

open Utils

let solution =
    let isPalindrome n =
        let nStr = n.ToString()
        let nStrRev = nStr |> Seq.rev |> Seq.map string |> String.concat ""
        nStr = nStrRev

    let numbers = [ 100 .. 999 ]

    let candidates =
        List.map (fun (x: int list) -> x.[0] * x.[1]) (cartesian [ numbers; numbers ])

    candidates |> Seq.filter isPalindrome |> Seq.max

printfn "The largest palindrome made from the product of two 3-digit numbers is %A" solution
