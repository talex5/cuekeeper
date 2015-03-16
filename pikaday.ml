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
  end

let make_config () : config Js.t = Js.Unsafe.obj [| |]

let pikaday_constr : (config Js.t -> pikaday Js.t) Js.constr = Js.Unsafe.global##_Pikaday

let make ~on_select =
  let div = Html5.div [] in
  let elem = Tyxml_js.To_dom.of_div div in
  let config = make_config () in
  config##container <- elem;
  config##onSelect <- Js.wrap_callback (fun d -> on_select (d##valueOf () /. 1000.));
  let pd = jsnew pikaday_constr (config) in
  ignore pd;
  div
