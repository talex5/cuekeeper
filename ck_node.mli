(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

(** In-memory representation of the nodes.
 * This extends Ck_disk_node with some calculated values. *)

open Ck_sigs

module SortKey : Slow_set.SORT_KEY
module M : Map.S with type key = SortKey.t

type +'a t = {
  uuid : Ck_id.t;
  disk_node : 'a Ck_disk_node.t;
  child_nodes : generic M.t;
}
and generic = [action | project | area] t

include DISK_NODE
  with type 'a t := 'a t

val make : uuid:Ck_id.t -> disk_node:'a Ck_disk_node.t -> child_nodes:generic M.t -> 'a t
val make_root : child_nodes:generic M.t -> [> area] t

val uuid : _ t  -> Ck_id.t
val child_nodes : _ t  -> generic M.t

val key : _ t -> SortKey.t
(** A key for sorting by name. *)

val equal : generic -> generic -> bool
