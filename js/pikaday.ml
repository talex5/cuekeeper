(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

(** Bindings for Pikadate.
 * See https://github.com/dbushell/Pikaday/ *)

open Js_of_ocaml
open Js_of_ocaml_tyxml.Tyxml_js

class type pikaday =
  object
    method getDate : Js.date Js.t Js.Opt.t Js.meth
  end

class type config =
  object
    method container : Dom_html.element Js.t Js.prop
    method onSelect : (pikaday, Js.date Js.t -> unit) Js.meth_callback Js.prop
    method defaultDate : Js.date Js.t Js.Optdef.t Js.prop
    method setDefaultDate : bool Js.t Js.prop
  end

let make_config () : config Js.t = Js.Unsafe.obj [| |]

let pikaday_constr : (config Js.t -> pikaday Js.t) Js.constr = Js.Unsafe.global##._Pikaday

let to_user_date d =
  Ck_time.make
    ~year:d##getFullYear
    ~month:d##getMonth
    ~day:d##getDate

let make ?(initial:Ck_time.user_date option) ~on_select () =
  let div = Html5.div [] in
  let elem = To_dom.of_div div in
  let config = make_config () in
  config##.container := elem;
  config##.onSelect := Js.wrap_callback (fun d ->
    on_select (to_user_date d)
  );
  begin match (initial :> (int * int * int) option) with
  | Some (y, m, d) ->
      let js_date = new%js Js.date_day y m d in
      config##.defaultDate := Js.Optdef.return js_date;
      config##.setDefaultDate := Js._true;
  | None -> () end;
  let pd = new%js pikaday_constr config in
  (div, pd)

let get_date (pd : #pikaday Js.t) =
  Js.Opt.case pd##getDate
    (fun () -> None)
    (fun d -> Some (to_user_date d))
