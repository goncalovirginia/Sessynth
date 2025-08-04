
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

let rec choice_map_list f xs =
    let open Let_syntax in
    match xs with
    | [] -> Choice.return []
    | x :: xs' ->
        let* y = f x in
        let* ys = choice_map_list f xs' in
        Choice.return (y :: ys)