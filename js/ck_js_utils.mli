(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

val ignore_listener : Dom_html.event_listener_id -> unit
(** Version of [ignore] restricted to event listeners. *)

val inside : #Dom_html.element Js.t -> #Dom_html.element Js.t -> bool
(** [inside parent child] is true if [child] or one of its ancestors is [parent]. *)

val keycode_escape : int

val pos_from_root : #Dom_html.element Js.t -> (int * int)
(** Find the absolute position of an element. *)

val async : name:string -> (unit -> unit Lwt.t) -> unit
(** Wrapper for [Lwt_js_events.async] that logs exceptions with the given label.
 * Also, it forces the return type to be [unit Lwt.t]. *)

val auto_focus : [< Html5_types.input] Tyxml_js.Html5.elt -> unit
(** [auto_focus i] gives [i] the focus after this turn (giving it a chance to be rendered first). *)
