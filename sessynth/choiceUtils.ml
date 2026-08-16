
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

let rec map_list_state f g xs =
    match xs with
    | [] -> Choice.return (f, [])
    | x :: xs' ->
        let* (f', y) = g f x in
        let* (f'', ys) = map_list_state f' g xs' in
        Choice.return (f'', y :: ys)

(* Bridges a partial operation into the search: [Some x] succeeds with [x],
   [None] fails the branch.

   Choice is a CPS monad, so the continuation of a [let*] runs when the search is
   forced by run_n/run_all, long after the enclosing OCaml expression has been
   evaluated. A [try ... with] wrapped around a [let*] therefore protects only
   the *construction* of the search, and an exception raised inside the
   continuation escapes it. Partial operations must return an option and be
   bound through this, rather than raise and be caught. *)
let of_option o =
    match o with
    | Some x -> Choice.return x
    | None -> Choice.fail

let mplus_list xs =
  	List.fold_right Choice.mplus xs Choice.fail

let map_mplus_list f xs =
	mplus_list (List.map f xs)