
module Syntax = struct

  (* monadic bind (dependent sequencing) *)
  let ( let* ) m f = Choice.bind f m

  (* applicative map (mapping over results) *)
  let ( let+ ) m f = Choice.map f m

  (* applicative pairing (independent parallel operations) *)
  let ( and+ ) a b =
    Choice.bind (fun x ->
      Choice.map (fun y ->
        (x, y)) b) a

end

module Let_syntax = Syntax

open Let_syntax

let rec map_list f xs =
    match xs with
    | [] -> Choice.return []
    | x :: xs' ->
        let* y = f x in
        let* ys = map_list f xs' in
        Choice.return (y :: ys)

let mplus_list xs =
  List.fold_right Choice.mplus xs Choice.fail

let map_mplus_list f xs =
	mplus_list (List.map f xs)