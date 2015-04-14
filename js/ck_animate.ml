(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

(* Ck_model gives us 1s for the whole fade-out, resize, fade-in sequence *)
let fade_time = 0.33
let resize_time = 0.33

let scroll_time = 0.25

(* (forces return type to be unit) *)
let async : (unit -> unit Lwt.t) -> unit = Lwt_js_events.async

let clamp lo hi v =
  min (max v lo) hi

(* Fade out, then animate max-height down to zero. Can't do this with CSS because of FF bug #830056.
 * [when_complete] is called on success (but not if cancelled). *)
let fade_out ?when_complete elem =
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
      else 
        match when_complete with
        | None -> ()
        | Some fn -> fn ()
    ) in
  aux ();
  fun () ->
    cancelled := true;
    elem##style##opacity <- Js.string "" |> Js.Optdef.return;
    elem##style##maxHeight <- Js.string ""

(* Runs in parallel with fade_out.
 * Wait for fade_time (original disappears), then expand this item as the other
 * one shrinks, then fade in. *)
let fade_in_move ~full_height elem =
  let open Lwt in
  let cancelled = ref false in
  let start = Unix.gettimeofday () +. fade_time in
  let fade_start = start +. resize_time in
  let rec aux () =
    if not !cancelled then (
      let t = Unix.gettimeofday () in
      let fade_frac = (t -. fade_start) /. fade_time in
      let grow_frac = (t -. start) /. resize_time in
      let o = fade_frac |> clamp 0.0 1.0 in
      let h = float_of_int full_height *. grow_frac |> truncate in
      let h = h |> clamp 0 full_height in
      let () =
        let elem = Tyxml_js.To_dom.of_element elem in
        let o = Printf.sprintf "%g" o in
        elem##style##opacity <- Js.string o |> Js.Optdef.return;
        elem##style##maxHeight <- (Js.string (string_of_int h ^ "px")) in
      if o < 1.0 then
        Dom_html._requestAnimationFrame (Js.wrap_callback aux)
    ) in
  async (fun () -> Lwt_js.sleep fade_time >|= fun () ->
    (* By this time, we've had a chance to fill in the height of the removed item. *)
    aux ()
  );
  fun () ->
    cancelled := true;
    let elem = Tyxml_js.To_dom.of_element elem in
    elem##style##opacity <- Js.string "" |> Js.Optdef.return;
    elem##style##maxHeight <- Js.string ""

let animate_scroll_to target_y =
  let start = Unix.gettimeofday () in
  let _, start_y = Dom_html.getDocumentScroll () in
  let root = Dom_html.document##documentElement in
  let rec aux () =
    let f = (Unix.gettimeofday () -. start) /. scroll_time |> min 1.0 in
    let dy = float_of_int (target_y - start_y) *. f in
    root##scrollTop <- start_y + truncate dy;
    if f < 1.0 then Dom_html._requestAnimationFrame (Js.wrap_callback aux) in
  aux ()

(** Animate scrolling the window so that the range (top, bottom) is visible. *)
let scroll_to_show (top, bottom) =
  Js.Optdef.iter (Dom_html.window##innerHeight) (fun page_height ->
    let region_height = (bottom - top + 2) in
    let region_height = min region_height page_height in
    let _left, scroll_top = Dom_html.getDocumentScroll () in
    if top < scroll_top then (
      animate_scroll_to top
    ) else if top + region_height > scroll_top + page_height then (
      (* Put top + region_height at the bottom of the viewport *)
      animate_scroll_to (top + region_height - page_height)
    )
  )
