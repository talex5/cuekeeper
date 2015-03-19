(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

(** Bindings for Pikadate.
 * See https://github.com/dbushell/Pikaday/ *)

open Tyxml_js

class type pikaday =
  object
  end

class type config =
  object
    method container : Dom_html.element Js.t Js.prop
    method onSelect : (pikaday, Js.date Js.t -> unit) Js.meth_callback Js.prop
    method defaultDate : Js.date Js.t Js.Optdef.t Js.prop
    method setDefaultDate : bool Js.t Js.prop
  end

let make_config () : config Js.t = Js.Unsafe.obj [| |]

let pikaday_constr : (config Js.t -> pikaday Js.t) Js.constr = Js.Unsafe.global##_Pikaday

let make ?initial ~on_select () =
  let div = Html5.div [] in
  let elem = Tyxml_js.To_dom.of_div div in
  let config = make_config () in
  config##container <- elem;
  config##onSelect <- Js.wrap_callback (fun d ->
    let date, _tm = Unix.mktime { Unix.
      tm_year = d##getFullYear () - 1900;
      tm_mon = d##getMonth ();
      tm_mday = d##getDate ();
      tm_hour = 0;
      tm_min = 0;
      tm_sec = 0;
      (* (these are ignored) *)
      tm_isdst = false;
      tm_wday = 0;
      tm_yday = 0;
    } in
    on_select date
  );
  begin match initial with
  | Some date_utc ->
      let open Unix in
      let local_date = localtime date_utc in
      let js_date = jsnew Js.date_day (local_date.tm_year + 1900, local_date.tm_mon, local_date.tm_mday) in
      config##defaultDate <- Js.Optdef.return js_date;
      config##setDefaultDate <- Js._true;
  | None -> () end;
  let pd = jsnew pikaday_constr (config) in
  ignore pd;
  div
