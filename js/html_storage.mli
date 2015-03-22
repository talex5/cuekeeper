(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

type t
type key = string

val make : unit -> t
val get : t -> key -> string option
val set : t -> key -> string -> unit
val remove : t -> key -> unit

val watch : t -> prefix:string -> (key -> string option -> unit) -> Dom.event_listener_id
