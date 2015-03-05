(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Lwt

type state =
  [ `New
  | `Current
  | `Removed of float ] (* Time item was removed from input *)

module type SORT_KEY = sig
  include Map.OrderedType
  module Id : Map.OrderedType
  val id : t -> Id.t
  val show : t -> string
end

type 'a item = {
  data : 'a;
  state : state React.S.t;
  set_state : state -> unit;
}

let data item = item.data
let state item = item.state

let make_item initial_state data =
  let state, set_state = React.S.create initial_state in
  {data; state; set_state}

module Make (C : Ck_clock.S) (K : SORT_KEY) (M : Map.S with type key = K.t) = struct
  module Id_map = Map.Make(K.Id)

  let diff_old_new ~eq ~removed_by_id ~time key i_old i_new =
    match i_old, i_new with
    | None, None -> None
    | Some _, None ->
        removed_by_id := !removed_by_id |> Id_map.add (K.id key) key;
        Some (`Removed time)
    | None, Some n -> Some (`New n)
    | Some o, Some n when not (eq o n) -> Some (`Updated n)
    | Some _, Some _ -> None

  let merge_diff _key prev patch =
    match prev, patch with
    | prev, None -> prev
    | Some old as prev, Some ((`Removed _) as r) -> old.set_state r; prev
    | Some old, Some (`Updated data) -> Some {old with data}
    | None, Some (`New data) -> Some (make_item `New data)
    | None, Some (`Renamed data) -> Some (make_item `Current data)
    | Some old, Some (`New data | `Renamed data) -> old.set_state `Current; Some {old with data}
    | Some _, Some `Drop -> None
    | None, Some (`Updated _ | `Removed _ | `Drop) -> assert false

  let adjacent current current_k old_k =
    let before_old, _, after_old = M.split old_k current in
    try
      let (adj_key, _) =
        if K.compare current_k old_k < 0 then M.max_binding before_old
        else M.min_binding after_old in
      K.compare adj_key current_k = 0
    with Not_found -> false

  (* Modify the diff:
   * - If a New item has the same ID as a Removed one then
   *   - If it's next to the old one, turn the pair into a `Drop, `Rename pair.
   *   - Otherwise, into a `Remove, `New pair.
   *)
  let detect_moves ~input ~removed_by_id diff =
    let renamed_src = ref [] in
    let diff = diff
      |> M.mapi (fun k v ->
        match v with
        | (`New data) as p ->
            begin try
              let src_key = Id_map.find (K.id k) removed_by_id in
              if adjacent input k src_key then (
                renamed_src := src_key :: !renamed_src; `Renamed data
              ) else p
            with Not_found -> p end
        | p -> p
      )
      |> ref in
    !renamed_src |> List.iter (fun src_key ->
      diff := !diff |> M.add src_key `Drop
    );
    !diff

  let make ~delay ~eq ?init input =
    let init =
      match init with
      | None -> React.S.value input
      | Some i -> i in

    let out_item_eq a b = eq a.data b.data in

    let output, set_output =
      init
      |> M.map (make_item `Current)
      |> React.S.create ~eq:(M.equal out_item_eq) in

    (* Called [delay] after some items have been added or removed.
     * Update their status if appropriate. *)
    let check start delayed =
      let m = ref (React.S.value output) in
      delayed |> M.iter (fun k -> function
        | `Drop | `Renamed _ | `Updated _ -> ()
        | `New _ ->
            begin try
              let item = M.find k !m in
              begin match React.S.value item.state with
                | `New -> item.set_state `Current
                | `Current | `Removed _ -> () end
            with Not_found ->
              (* Can happen if we add and remove quickly and the remove callback
               * arrives first. *)
              ()
            end
        | `Removed _ ->
            let item = M.find k !m in
            begin match React.S.value item.state with
            | `Removed t when t = start -> m := !m |> M.remove k
            | `New | `Current | `Removed _ -> () end
      );
      set_output !m in

    let keep_me =
      input |> React.S.diff (fun s_new s_old ->
        let time = C.now () in
        let removed_by_id = ref Id_map.empty in
        let diff =
          M.merge (diff_old_new ~eq ~removed_by_id ~time) s_old s_new
          |> detect_moves ~input:s_new ~removed_by_id:!removed_by_id in

        if not (M.is_empty diff) then (
          M.merge merge_diff (React.S.value output) diff
          |> set_output;
          C.async ~name:"slow_set" (fun () ->
            C.sleep delay >|= fun () ->
            check time diff
          )
        )
      ) in
    React.S.retain output (fun () -> ignore keep_me) |> ignore;

    output
end
