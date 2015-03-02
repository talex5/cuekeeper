(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

(** A single revision in the Irmin history. *)

open Ck_sigs

module Make(I : Irmin.BASIC with type key = string list and type value = string) : sig
  type t

  type commit = float * string

  val make : master:(string -> I.t) -> (string -> I.t) -> t Lwt.t
  val equal : t -> t -> bool

  val root : t -> [> area] Ck_node.t
  val history : t -> commit list  (* XXX: recent *)

  val get : t -> Ck_id.t -> Ck_node.generic option

  (*XXX: uuid *)
  val add : t -> ?uuid:Ck_id.t -> [< action | project | area] -> parent:Ck_id.t -> name:string -> description:string -> Ck_id.t Lwt.t
  val delete : t -> [< action | project | area] Ck_node.t -> unit Lwt.t

  val set_name : t -> [< action | project | area] Ck_node.t -> string -> unit Lwt.t
  val set_details : t -> [< action | project | area] Ck_node.t -> [< action | project | area] -> unit Lwt.t
  val set_starred : t -> [< action | project] Ck_node.t -> bool -> unit Lwt.t
  val set_action_state : t -> [action] Ck_node.t -> [ `Next | `Waiting | `Future | `Done ] -> unit Lwt.t
  val set_project_state : t -> [project] Ck_node.t -> [ `Active | `SomedayMaybe | `Done ] -> unit Lwt.t
end
