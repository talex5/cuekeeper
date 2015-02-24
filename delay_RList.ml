(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open ReactiveData.RList
open Lwt

module type CLOCK = sig
  val now : unit -> float
  val async : (unit -> unit Lwt.t) -> unit
  val sleep : float -> unit Lwt.t
end

type pending = {
  time : float;       (* When to actually delete it *)
  mutable i : int;    (* Current index in list. *)
}

let by_index a b =
  compare a.i b.i

type set_state = [ `New | `Current | `Removed ] -> unit

type 'a item = {
  set_state : set_state;
  state : [ `New | `Current | `Removed ] React.S.t;
  data : 'a;
}

let current = React.S.const `Current
let fixed data = {
  state = current; set_state = ignore; data
}

let make_item state data =
  let state, set_state = React.S.create state in
  { state; set_state; data }

module Make (C : CLOCK) = struct
  let make ~delay src =
    let src_events = ReactiveData.RList.event src in
    let delete_events, send_delete = React.E.create () in
    let pending_deletes = ref [] in
    let adjust i =
      let i =
        if i < 0 then i + List.length (value src) else i in
      (* Source wants to change index i. Undo changes due to pending deletes. *)
      let rec aux i = function
        | [] -> i
        | d::ds ->
            if i >= d.i then aux (i + 1) ds
            else aux i ds in
      aux i (List.sort by_index !pending_deletes) in

    let thread = ref None in
    let rec schedule () : unit =
      match !thread, !pending_deletes with
      | None, d::ds ->
          let interval = d.time -. C.now () in
          if interval <= 0.0 then (
            send_delete (Patch [R d.i]);
            ds |> List.iter (fun other_d ->
              if other_d.i > d.i then other_d.i <- other_d.i - 1
            );
            pending_deletes := ds;
            schedule ()
          ) else (
            thread := Some (C.sleep interval >|= fun () ->
              thread := None;
              schedule ()
            )
          )
      | _ -> () in

    let current_dst = ref ReactiveData.RList.empty in

    let filtered_events = src_events |> React.E.map (function
      | Set s ->
          pending_deletes := [];
          begin match !thread with
          | None -> ()
          | Some t ->
              Lwt.cancel t;
              thread := None
          end;
          Set (s |> List.map (make_item `Current))
      | Patch ps ->
          let ps = ps |> Ck_utils.filter_map (function
            | I (i, x) ->
                let i = adjust i in
                !pending_deletes |> List.iter (fun d ->
                  if d.i >= i then d.i <- d.i + 1
                );
                let item = make_item `New x in
                let no_longer_new = C.sleep delay in
                C.async (fun () ->
                  no_longer_new >|= fun () ->
                  if React.S.value item.state = `New then item.set_state `Current;
                );
                Some (I (i, item))
            | R i ->
                let i = (* Need to add one as src has already been shortened. *)
                  if i < 0 then i + List.length (value src) + 1 else i in
                let i = adjust i in
                pending_deletes := !pending_deletes @ [{ time = C.now () +. delay; i}];
                (List.nth (value !current_dst) i).set_state `Removed;
                None
            | U (i, x) ->
                let i = adjust i in
                Some (U (i, make_item `Current x))
            | X (i, offset) ->
                let dst = adjust (i + offset) in
                let i = adjust i in
                if offset > 0 then (
                  !pending_deletes |> List.iter (fun d ->
                    if d.i > i && d.i < dst then d.i <- d.i - 1
                  )
                ) else (
                  !pending_deletes |> List.iter (fun d ->
                    if d.i > dst && d.i < i then d.i <- d.i + 1
                  )
                );
                Some (X (i, dst - i))
          ) in
          schedule ();
          Patch ps
    ) in
    (* (since delete_events always fires from a Lwt async thread, it can't overlap with filtered_events) *)
    let result = ReactiveData.RList.make_from
      (value src |> List.map (make_item `Current))
      (React.E.select [filtered_events; delete_events]) in
    current_dst := result;
    result
end
