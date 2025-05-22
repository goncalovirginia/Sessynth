open Language;;

let solve t =
    match t with
    | RTAnd(t1, t2) -> t1 = t2
    | _ -> false