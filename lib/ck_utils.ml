(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

let (>>~=) x f = React.S.bind x f
let (>|~=) x f = React.S.map f x

let (>|?=) x f =
  match x with
  | None -> None
  | Some x -> Some (f x)

let default d = function
  | None -> d
  | Some x -> x

let bug fmt =
  let do_raise msg = raise @@ Failure msg in
  Printf.ksprintf do_raise fmt

module StringMap = struct
  include Map.Make(String)
  let find_nf = find
  let find_safe key map = try find key map with Not_found -> bug "BUG: Key '%s' not found in StringMap!" key
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

(* Get the index of an item in an assoc list. *)
let index_of key items =
  let rec aux i = function
    | [] -> None
    | (k, _v) :: _ when k = key -> Some i
    | _ :: xs -> aux (i + 1) xs in
  aux 0 items

let tail s i =
  String.sub s i (String.length s - i)

module UTF8_codec : sig
  (* If the data is valid UTF-8 then store it directly (prefixed with '"').
   * Otherwise, encode with Base64 (and prefix with "%"). *)
  val encode : string -> string
  val decode : string -> string
end = struct
  (* From https://github.com/mirage/ezjsonm.
   * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org> *)
  let is_valid_utf8 str =
    try
      Uutf.String.fold_utf_8 (fun () _ -> function
        | `Malformed _ -> raise Exit
        | _ -> ()
      ) () str;
      true
    with Exit -> false

  let encode s =
    if is_valid_utf8 s then "\"" ^ s
    else "%" ^ B64.encode s

  let decode s =
    match s.[0] with
    | '%' -> B64.decode (tail s 1)
    | '"' -> tail s 1
    | _ -> B64.decode s            (* Old format, base64 *)
end

