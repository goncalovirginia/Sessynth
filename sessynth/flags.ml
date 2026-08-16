(* Search state threaded through every rule: fresh-name supplies, the fuel
   budget, and debug output settings. Independent of the types and contexts. *)

open Language

type fresh_indices = { id : int; func : int; chan : int; kind : int }

type flags = {
    xRecLam : id;
    freshIndices : fresh_indices;
    usedFuel : int;
    maxFuel : int;
    printDebug : bool
}

let initialize_flags maxFuel printDebug =
    {
        xRecLam = "";
        freshIndices = { id = 0; func = 0; chan = 0; kind = 0 };
        usedFuel = -1;
        maxFuel = maxFuel;
        printDebug = printDebug
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

(* increments one unit of the search budget, failing the branch once it runs out *)
let consume_fuel f =
    if f.usedFuel < f.maxFuel then Choice.return { f with usedFuel = f.usedFuel + 1 }
    else Choice.fail
