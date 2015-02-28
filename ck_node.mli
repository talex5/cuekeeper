(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

(** In-memory representation of the nodes.
 * This extends Ck_disk_node with some calculated values. *)

open Ck_sigs

module SortKey : Slow_set.SORT_KEY
module M : Map.S with type key = SortKey.t

include DISK_NODE

val make : uuid:Ck_id.t -> disk_node:'a Ck_disk_node.t -> child_nodes:generic M.t -> 'a t
val make_root : child_nodes:generic M.t -> [> area] t

val uuid : [< area | project | action] t -> Ck_id.t
val child_nodes : 'a t  -> generic M.t
val disk_node : 'a t -> 'a Ck_disk_node.t

val key : _ t -> SortKey.t
(** A key for sorting by name. *)

val equal : generic -> generic -> bool

val ty : [< action | project | area] t ->
  [ `Action of [> action] t
  | `Project of [> project] t
  | `Area of [> area] t ]

val with_name : 'a t -> string -> 'a t
val with_details : _ t -> 'a -> 'a t
