[<AutoOpen>]
module Collections =
    let allOf (pred: 'T -> bool) (xs: seq<'T>) =
        Seq.tryFind (pred >> not) xs |> Option.isNone

    let anyOf (pred: 'T -> bool) (xs: seq<'T>) =
        Seq.tryFind pred xs |> Option.isSome

    let noneOf (pred: 'T -> bool) (xs: seq<'T>) =
        Seq.tryFind pred xs |> Option.isNone

    let rec cartesian lstlst =
        match lstlst with
        | [ h ] -> List.fold (fun acc elem -> [ elem ] :: acc) [] h
        | h :: t ->
            List.fold (fun cacc celem ->
                (List.fold (fun acc elem -> (elem :: celem) :: acc) [] h)
                @ cacc) [] (cartesian t)
        | _ -> []


[<AutoOpen>]
module Math =
    let isFactorOf x n = n % x = 0

    let isAnyFactorOf (xs: int list) n = anyOf (fun x -> isFactorOf x n) xs

    let isAllFactorOf (xs: int list) n = allOf (fun x -> isFactorOf x n) xs

    let primeFactors n =
        let rec getFactor num proposed acc =
            if proposed = num then proposed :: acc
            elif num % proposed = 0L then getFactor (num / proposed) proposed (proposed :: acc)
            else getFactor num (proposed + 1L) acc

        getFactor n 2L []

    let rec gcd x y = if y = 0 then abs x else gcd y (x % y)

    let lcm x y = x * y / (gcd x y)

    let lcmMany xs =
        xs
        |> List.fold (fun x y -> (lcm x y)) 1

    let isPrime n =
        let nsqrt = n |> float |> System.Math.Sqrt |> int
        noneOf (fun x -> n % x = 0) [2..nsqrt]

    let primes =
        Seq.unfold (fun i -> Some (i, i + 1)) 2
        |> Seq.filter isPrime