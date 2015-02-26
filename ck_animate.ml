(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

(* Ck_model gives us 1s for the whole fade-out, resize, fade-in sequence *)
let fade_time = 0.33
let resize_time = 0.33

(* (forces return type to be unit) *)
let async : (unit -> unit Lwt.t) -> unit = Lwt_js_events.async

let clamp lo hi v =
  min (max v lo) hi

(* Fade out, then animate max-height down to zero. Can't do this with CSS because of FF bug #830056 *)
let fade_out elem =
  let cancelled = ref false in
  let start = Unix.gettimeofday () in
  let shrink_start = start +. resize_time in
  let full_height = float_of_int elem##offsetHeight in
  let rec aux () =
    if not !cancelled then (
      let t = Unix.gettimeofday () in
      let fade_frac = (t -. start) /. fade_time in
      let shrink_frac = (t -. shrink_start) /. resize_time in
      let o = max 0.0 (1. -. fade_frac) in
      let o = Printf.sprintf "%g" o in
      let h = full_height *. (1. -. shrink_frac) |> truncate in
      let h = max 0 h in
      elem##style##opacity <- Js.string o |> Js.Optdef.return;
      elem##style##maxHeight <- (Js.string (string_of_int h ^ "px"));
      if h > 0 then
        Dom_html._requestAnimationFrame (Js.wrap_callback aux)
    ) in
  aux ();
  fun () ->
    cancelled := true;
    elem##style##opacity <- Js.string "" |> Js.Optdef.return;
    elem##style##maxHeight <- Js.string ""

(* Runs in parallel with fade_out.
 * Wait for fade_time (original disappears), then expand this item as the other
 * one shrinks, then fade in. *)
let fade_in_move ~full_height item_ref =
  let open Lwt in
  let cancelled = ref false in
  let start = Unix.gettimeofday () +. fade_time in
  let fade_start = start +. resize_time in
  let rec aux full_height () =
    if not !cancelled then (
      let t = Unix.gettimeofday () in
      let fade_frac = (t -. fade_start) /. fade_time in
      let grow_frac = (t -. start) /. resize_time in
      let o = fade_frac |> clamp 0.0 1.0 in
      let h = float_of_int full_height *. grow_frac |> truncate in
      let h = h |> clamp 0 full_height in
      begin match !item_ref with
      | None -> ()
      | Some elem ->
          let elem = Tyxml_js.To_dom.of_element elem in
          let o = Printf.sprintf "%g" o in
          elem##style##opacity <- Js.string o |> Js.Optdef.return;
          elem##style##maxHeight <- (Js.string (string_of_int h ^ "px")) end;
      if o < 1.0 then
        Dom_html._requestAnimationFrame (Js.wrap_callback (aux full_height))
    ) in
  async (fun () -> Lwt_js.sleep fade_time >|= fun () ->
    (* By this time, we've had a chance to fill in the height of the removed item. *)
    let full_height =
      match !full_height with
      | None -> print_endline "WARNING: missing height in fade_in_move!"; 24
      | Some h -> h in
    aux full_height ()
  );
  fun () ->
    cancelled := true;
    match !item_ref with
    | None -> ()
    | Some elem ->
        let elem = Tyxml_js.To_dom.of_element elem in
        elem##style##opacity <- Js.string "" |> Js.Optdef.return;
        elem##style##maxHeight <- Js.string ""
