[<AutoOpen>]
module Utils =

    let allOf (pred: 'T -> bool) (xs: seq<'T>) =
        Seq.tryFind (pred >> not) xs |> Option.isNone

    let anyOf (pred: 'T -> bool) (xs: seq<'T>) = Seq.tryFind pred xs |> Option.isSome

    let noneOf (pred: 'T -> bool) (xs: seq<'T>) = Seq.tryFind pred xs |> Option.isNone

    let isFactorOf x n = n % x = 0

    let isAnyFactorOf (xs: int list) n = anyOf (fun x -> isFactorOf x n) xs

    let isAllFactorOf (xs: int list) n = allOf (fun x -> isFactorOf x n) xs


    let primeFactors n =
        let rec getFactor num proposed acc =
            if proposed = num then proposed :: acc
            elif num % proposed = 0L then getFactor (num / proposed) proposed (proposed :: acc)
            else getFactor num (proposed + 1L) acc

        getFactor n 2L []


    let rec cartesian lstlst =
        match lstlst with
        | [ h ] -> List.fold (fun acc elem -> [ elem ] :: acc) [] h
        | h :: t ->
            List.fold (fun cacc celem ->
                (List.fold (fun acc elem -> (elem :: celem) :: acc) [] h)
                @ cacc) [] (cartesian t)
        | _ -> []
