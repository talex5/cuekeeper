(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

(** An Irmin backend that stores data in an IndexedDB. *)

(* [config dn_name] store all values in the given IndexedDB database. *)
val config : string -> Irmin.config

module AO: Irmin.AO_MAKER
module RW: Irmin.RW_MAKER
module Make: Irmin.S_MAKER
