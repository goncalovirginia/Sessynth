(* Search state threaded through every rule: the fresh-name supplies and how deep
   into a derivation they are. Independent of the types and contexts. *)

let default_max_depth = 100
let max_depth = ref default_max_depth
let set_max_depth n = if n > 0 then max_depth := n

let print_debug = ref false
let set_print_debug b = print_debug := b

type fresh_indices = { id : int; func : int; chan : int; kind : int }

type flags = {
    freshIndices : fresh_indices;
    depth : int
}

let initialize_flags () =
    {
        freshIndices = { id = 0; func = 0; chan = 0; kind = 0 };
        depth = -1
    }

let fresh_id f =
    let curr_id = f.freshIndices.id in
    let f' = { f with freshIndices = { f.freshIndices with id = curr_id + 1 } } in
    f', ("_x" ^ string_of_int curr_id)

let fresh_fun f =
    let curr_fun = f.freshIndices.func in
    let f' = { f with freshIndices = { f.freshIndices with func = curr_fun + 1 } } in
    f', ("_f" ^ string_of_int curr_fun)

let fresh_chan f =
    let curr_chan = f.freshIndices.chan in
    let f' = { f with freshIndices = { f.freshIndices with chan = curr_chan + 1 } } in
    f', ("_c" ^ string_of_int curr_chan)

let fresh_kind f =
    let curr_kind = f.freshIndices.kind in
    let f' = { f with freshIndices = { f.freshIndices with kind = curr_kind + 1 } } in
    f', ("_α" ^ string_of_int curr_kind)

let increment_depth f =
    if f.depth < !max_depth then Choice.return { f with depth = f.depth + 1 }
    else Choice.fail
