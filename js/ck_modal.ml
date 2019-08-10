(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Js_of_ocaml
open Ck_js_utils

type t = {
  element : Dom_html.element Js.t;
  close : unit -> unit;
}

let current = ref None

let close () =
  match !current with
  | None -> ()
  | Some t ->
      current := None;
      t.close ()

let show ~close:c element =
  close ();
  current := Some {
    element = (element :> Dom_html.element Js.t);
    close = c;
  }

(* Listen to global clicks and keypresses so we can close modals on click/escape *)
let () =
  let click (ev:#Dom_html.mouseEvent Js.t) =
    match !current with
    | None -> Js._true
    | Some modal ->
        Js.Opt.case (ev##.target)
          (fun () -> Js._true)
          (fun target ->
            if target |> inside modal.element then (
              (* Click inside modal - pass it on *)
              Js._true
            ) else (
              (* Click outside modal; close the modal *)
              close ();
              Dom_html.stopPropagation ev;
              Js._false
            )
          ) in
  let keyup ev =
    match !current with
    | Some _ when ev##.keyCode = keycode_escape ->
        close ();
        Dom_html.stopPropagation ev;
        Js._false
    | _ -> Js._true in
  Dom_html.addEventListener Dom_html.document Dom_html.Event.click (Dom.handler click) Js._true |> ignore_listener;
  Dom_html.addEventListener Dom_html.document Dom_html.Event.keypress (Dom.handler keyup) Js._true |> ignore_listener
