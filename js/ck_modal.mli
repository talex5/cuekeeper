(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

(** Manage modal elements. A modal element is automatically closed when:
 * - another modal is shown,
 * - escape is pressed, or
 * - the user clicks outside the modal element *)

val show : close:(unit -> unit) -> #Dom_html.element Js.t -> unit
(** Show a new modal (closing any currently-open one first). *)

val close : unit -> unit
(** Close any currently open modal. *)
