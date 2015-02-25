(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Lwt

type state =
  [ `New
  | `Moved
  | `Current
  | `Removed of float ] (* Time item was removed from input *)

module type ITEM = sig
  include Map.OrderedType
  val id : t -> Ck_id.t
end

module Make (C : Ck_clock.S) (I : ITEM) (M : Map.S with type key = I.t) = struct
  let diff_old_new ~removed_by_id ~time key i_old i_new =
    match i_old, i_new with
    | None, None -> None
    | Some o, None ->
        removed_by_id := !removed_by_id |> Ck_id.M.add (I.id key) o;
        Some (o, `Removed time)
    | None, Some n -> Some (n, `New)
    | Some o, Some n when o <> n -> Some (n, `Updated)
    | Some _, Some _ -> None

  let merge_diff _key prev = function
    | None -> prev
    | Some (n, `Updated) ->
        begin match prev with
        | Some (_o, state) -> Some (n, state)
        | None -> assert false end
    | Some (_, (`Removed _ | `New | `Moved)) as n -> n

  let detect_moves ~removed_by_id =
    M.mapi (fun k v ->
      match v with
      | (v, `New) as p ->
          begin try
            let _old = Ck_id.M.find (I.id k) removed_by_id in
            (v, `Moved)
          with Not_found -> p end
      | p -> p
    )

  let make ~delay ~eq ?init input =
    let init =
      match init with
      | None -> React.S.value input
      | Some i -> i in

    let out_item_eq (a, ast) (b, bst) =
      ast = bst &&
      eq a b in

    let output, set_output =
      init
      |> M.map (fun v -> (v, `Current))
      |> React.S.create ~eq:(M.equal out_item_eq) in

    (* Called [delay] after some items have been added or removed.
     * Update their status if appropriate. *)
    let check start delayed =
      let m = ref (React.S.value output) in
      delayed |> M.iter (fun k _v ->
        match M.find k !m with
        | (v, (`New | `Moved)) -> m := !m |> M.add k (v, `Current)
        | (_, `Removed t) when t = start -> m := !m |> M.remove k
        | (_, (`Current | `Removed _)) -> ()
      );
      set_output !m in

    let keep_me =
      input |> React.S.diff (fun s_new s_old ->
        let time = C.now () in
        let removed_by_id = ref Ck_id.M.empty in
        let diff =
          M.merge (diff_old_new ~removed_by_id ~time) s_old s_new
          |> detect_moves ~removed_by_id:!removed_by_id in

        if not (M.is_empty diff) then (
          M.merge merge_diff (React.S.value output) diff
          |> set_output;
          C.async (fun () ->
            C.sleep delay >|= fun () ->
            check time diff
          )
        )
      ) in
    React.S.retain output (fun () -> ignore keep_me) |> ignore;

    output
end
