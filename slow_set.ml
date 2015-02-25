(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Lwt

type state =
  [ `New
  | `Current
  | `Removed of float ] (* Time item was removed from input *)

module Make (C : Ck_clock.S) (Item : Set.OrderedType) (Input : Set.S with type elt = Item.t) = struct
  module M = Map.Make(Item)

  let eq : state M.t -> state M.t -> bool = M.equal (=)

  let make ~delay ?init input =
    let init =
      match init with
      | None -> React.S.value input
      | Some i -> i in

    let output, set_output =
      Input.fold (fun elt acc -> acc |> M.add elt `Current) init M.empty
      |> React.S.create ~eq in

    (* Called [delay] after some items have been added or removed.
     * Update their status if appropriate. *)
    let check start items =
      let m = ref (React.S.value output) in
      items |> Input.iter (fun i ->
        try
          match M.find i !m with
          | `New -> m := !m |> M.add i `Current
          | `Removed t when t = start -> m := !m |> M.remove i
          | `Current | `Removed _ -> ()
        with Not_found -> ()
      );
      set_output !m in

    let keep_me =
      input |> React.S.diff (fun s_new s_old ->
        let m = ref (React.S.value output) in
        let removed = Input.diff s_old s_new in
        removed |> Input.iter (fun i ->
          m := !m |> M.add i (`Removed (C.now ()))
        );
        let added = Input.diff s_new s_old in
        added |> Input.iter (fun i ->
          m := !m |> M.add i `New
        );
        let combined = Input.union added removed in
        if not (Input.is_empty combined) then (
          C.async (fun () ->
            let start = C.now () in
            C.sleep delay >|= fun () ->
            check start combined
          )
        );
        set_output !m
      ) in
    React.S.retain output (fun () -> ignore keep_me) |> ignore;

    output
end
