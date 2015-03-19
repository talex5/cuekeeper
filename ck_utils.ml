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
  (* TODO: this is not UTF-8 aware; it will only sort ASCII strings correctly. *)
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

let rlist_of ?init s =
  let init =
    match init with
    | None -> React.S.value s
    | Some v -> v in
  let changes = React.S.changes s |> React.E.map (fun x -> ReactiveData.RList.Set x) in
  ReactiveData.RList.make_from init changes

let string_of_day = function
  | 0 -> "Sun"
  | 1 -> "Mon"
  | 2 -> "Tue"
  | 3 -> "Wed"
  | 4 -> "Thu"
  | 5 -> "Fri"
  | 6 -> "Sat"
  | _ -> "XXX"

let fmt_date date =
  let open Unix in
  let tm = localtime date in
  Printf.sprintf "%04d-%02d-%02d (%s)"
    (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday (string_of_day tm.tm_wday)

let fmt_timestamp date =
  let open Unix in
  let tm = localtime date in
  Printf.sprintf "%04d-%02d-%02d %02d:%02d (%s)"
    (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min (string_of_day tm.tm_wday)

(* Get the index of an item in an assoc list. *)
let index_of key items =
  let rec aux i = function
    | [] -> None
    | (k, _v) :: _ when k = key -> Some i
    | _ :: xs -> aux (i + 1) xs in
  aux 0 items
