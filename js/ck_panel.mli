(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Tyxml_js

type t

val make :
  id:Ck_id.t ->
  closed:bool React.S.t ->          (* Becomes true when starting to close the box. *)
  set_closed:(bool -> unit) ->      (* Set to true if the user clicks the close icon. *)
  on_destroy:(unit -> unit) ->      (* Called when the fade-out is complete. *)
  title:[< Html_types.div_content_fun] Html5.elt ->
  contents:[< Html_types.div_content_fun] Html5.elt ->
  t

val highlight : Ck_id.t -> unit
val close : t -> unit
val element : t -> [`Div] Html5.elt
