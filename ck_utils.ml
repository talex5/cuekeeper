(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

let (>>~=) x f = React.S.bind x f
let (>|~=) x f = React.S.map f x

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

module Sort_key = struct
  type t = string * Ck_id.t
  let compare (a_name, a_id) (b_name, b_id) =
    match String.compare a_name b_name with
    | 0 -> compare a_id b_id
    | r -> r
  let id = snd
  let show = fst
end
module M = Map.Make(Sort_key)

let rec filter_map fn = function
  | [] -> []
  | (x::xs) ->
      match fn x with
      | None -> filter_map fn xs
      | Some y -> y :: filter_map fn xs

let rlist_of ~init s =
  let changes = React.S.changes s |> React.E.map (fun x -> ReactiveData.RList.Set x) in
  ReactiveData.RList.make_from init changes
