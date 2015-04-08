(* Copyright (C) 2015, Thomas Leonard.
 * See the README file for details. *)

(** Friendly OCaml/Lwt abstraction over IndexedDB. *)

type db
type db_upgrader
type key = string
type db_name = string
type store
type store_name

val store_name : string -> store_name

val make : db_name -> init:(db_upgrader -> unit) -> db Lwt.t
(** Connect to database [db_name]. If it doesn't yet exist, calls [init] to initialise it first. *)

val create_store : db_upgrader -> store_name -> unit

val store : db -> store_name -> store

val get : store -> key -> string option Lwt.t
val set : store -> key -> string -> unit Lwt.t

val compare_and_set : store -> key -> test:(string option -> bool) -> new_value:string option -> bool Lwt.t
(** If [test current_value] for returns true for the current value of [key], replace it with [new_value].
 * If [new_value] is None, the key is deleted.
 * This happens in a single atomic transaction. *)

val remove : store -> key -> unit Lwt.t
val bindings : store -> (key * string) list Lwt.t
