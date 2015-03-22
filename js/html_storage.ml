(* Copyright (C) 2015, Thomas Leonard.
 * See the README file for details. *)

open Ck_utils

class type storageEvent =
  object
    inherit Dom_html.event
    method key : Js.js_string Js.t Js.readonly_prop
    method oldValue : Js.js_string Js.t Js.opt Js.readonly_prop
    method newValue : Js.js_string Js.t Js.opt Js.readonly_prop
    method url : Js.js_string Js.t Js.readonly_prop
    method storageArea : Dom_html.storage Js.t Js.opt Js.readonly_prop
  end

type t = Dom_html.storage Js.t
type key = string

let make () =
  Js.Optdef.get (Dom_html.window##localStorage)
    (fun () -> failwith "HTML 5 storage is not available")

let get t key =
  Js.Opt.case (t##getItem (Js.string key))
    (fun () -> None)
    (fun v -> Some (Js.to_string v |> UTF8_codec.decode))

let set t key value =
  let encoded = UTF8_codec.encode value |> Js.string in
  t##setItem (Js.string key, encoded)

let remove t key =
  t##removeItem (Js.string key)

let event = Dom.Event.make "storage"

let key (ev:#storageEvent Js.t) = ev##key

let watch t ~prefix fn =
  let on_change (ev : storageEvent Js.t) =
    if ev##storageArea = Js.Opt.return t then (
      let k = key ev in
      if k##lastIndexOf_from (Js.string prefix, 0) = 0 then (
        let k = Js.to_string k in
        let v =
          Js.Opt.case (ev##newValue)
            (fun () -> None)
            (fun v ->
              Some (Js.to_string v |> UTF8_codec.decode)) in
        fn k v
      )
    );
    Js._true in
  Dom.addEventListener Dom_html.window event (Dom.handler on_change) Js._true
