(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

(** A single revision in the Irmin history. *)

open Ck_sigs

module Make(I : Irmin.BASIC with type key = string list and type value = string) : sig
  type t

  type commit = float * string

  val make : (string -> I.t) -> t Lwt.t
  val equal : t -> t -> bool

  val root : t -> [> area] Ck_node.t
  val history : t -> commit list  (* XXX: recent *)

  val get : t -> Ck_id.t -> Ck_node.generic option
  val get_exn : t -> Ck_id.t -> Ck_node.generic (* XXX *)

  (*XXX: uuid *)
  val add : t -> ?uuid:Ck_id.t -> [< action | project | area] -> parent:Ck_id.t -> name:string -> description:string -> (Ck_id.t * t) Lwt.t
  val delete : t -> [< action | project | area] Ck_node.t -> t Lwt.t

  val set_name : t -> [< action | project | area] Ck_node.t -> string -> t Lwt.t
  val set_details : t -> [< action | project | area] Ck_node.t -> [< action | project | area] -> t Lwt.t
  val set_starred : t -> [< action | project] Ck_node.t -> bool -> t Lwt.t
end
