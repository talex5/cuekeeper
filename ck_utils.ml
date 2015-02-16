(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

let error fmt =
  let do_raise msg = raise @@ Failure msg in
  Printf.ksprintf do_raise fmt

module StringMap = struct
  include Map.Make(String)
  let find_nf = find
  let find_safe key map = try find key map with Not_found -> error "BUG: Key '%s' not found in StringMap!" key
  let find key map = try Some (find key map) with Not_found -> None
  let map_bindings fn map = fold (fun key value acc -> fn key value :: acc) map []
end

module StringSet = Set.Make(String)

let rec filter_map fn = function
  | [] -> []
  | (x::xs) ->
      match fn x with
      | None -> filter_map fn xs
      | Some y -> y :: filter_map fn xs
