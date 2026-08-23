
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

(* Branches of a choice or a match are alternatives of one derivation, not a
   sequence: each is given the flags of the point they fork from, so the budget
   bounds the depth of a branch rather than the total work across them, and the
   fresh names a branch issues cannot reach its siblings. *)
let rec map_list f xs =
    match xs with
    | [] -> Choice.return []
    | x :: xs' ->
        let* y = f x in
        let* ys = map_list f xs' in
        Choice.return (y :: ys)

(** The first [n] distinct results of [c], compared structurally. More than one
    derivation can reach the same result, so filtering a truncated list after the
    fact would have spent slots on repeats; [Choice.iter] stops as soon as its
    callback says to, so [c] is only forced as far as [n] distinct results.
    Structural comparison means the results must be plain data. *)
let run_n_distinct n c =
    let seen = Hashtbl.create 100 and acc = ref [] and found = ref 0 in
    Choice.iter c (fun x ->
        if Hashtbl.mem seen x then true
        else begin
            Hashtbl.add seen x ();
            acc := x :: !acc;
            incr found;
            !found < n
        end);
    List.rev !acc

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