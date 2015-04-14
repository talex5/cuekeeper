(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Tyxml_js
open Html5
open Ck_utils
open Lwt

type t = {
  id : Ck_id.t;
  on_destroy : unit -> unit;
  set_closed : bool -> unit;
  element : [`Div] Html5.elt;
}

let async ~name (fn:unit -> unit Lwt.t) =
  Lwt_js_events.async (fun () ->
    Lwt.catch fn (fun ex ->
      Printf.printf "Async error in '%s'" name;
      Lwt.fail ex
    )
  )

let current_highlight, set_highlight = React.S.create None

(* Wrap contents in a panel with a close icon. *)
let make ~id ~closed ~set_closed ~on_destroy ~title ~contents =
  let elem = ref None in
  let cl =
    let cancel_close = ref ignore in
    closed >>~= (fun closed ->
      !cancel_close ();
      begin match closed, !elem with
      | true, Some elem ->
          let elem = Tyxml_js.To_dom.of_element elem in
          cancel_close := Ck_animate.fade_out ~when_complete:on_destroy elem
      | _ -> () end;
      current_highlight |> React.S.map (fun highlight ->
        let matches = (highlight = Some id) in
        begin match matches, !elem with
        | true, Some elem ->
            let elem = To_dom.of_element elem in
            let _x, y = Ck_js_utils.pos_from_root elem in
            let height = elem##clientHeight in
            Ck_animate.scroll_to_show (y, y + height);
        | _ -> () end;
        "ck-details" :: List.concat [
          if matches then ["ck-highlight"] else [];
          if closed then ["closed"] else [];
        ]
      )
    ) in
  let result = div ~a:[R.Html5.a_class cl] [
    a ~a:[a_onclick (fun _ -> set_closed true; true); a_class ["close"]] [entity "#215"];
    div ~a:[a_class ["ck-heading"]] [title];
    (contents :> [Html5_types.div_content_fun] Html5.elt);
  ] in
  elem := Some result;
  {
    id;
    on_destroy;
    set_closed;
    element = result;
  }

let highlight uuid =
  set_highlight (Some uuid);
  async ~name:"highlight" (fun () ->
    Lwt_js.sleep 2.0 >|= fun () ->
    if React.S.value current_highlight = Some uuid then set_highlight None
  )

let close t = t.set_closed true

let element t = t.element
