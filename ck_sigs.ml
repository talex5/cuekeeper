(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Sexplib.Std
open Ck_utils

type stop = unit -> unit

type action_details = {
  astarred : bool with default(false);
  astate : [ `Next | `Waiting | `Future | `Done ]
} with sexp

type project_details = {
  pstarred : bool with default(false);
  pstate : [ `Active | `SomedayMaybe | `Done ]
} with sexp

type action = [`Action of action_details]
type project = [`Project of project_details]
type area = [`Area]

module type DISK_NODE = sig
  type +'a t
  type generic = [ area | project | action ] t

  val parent : 'a t -> Ck_id.t
  val name : 'a t -> string
  val description : 'a t -> string
  val ctime : 'a t -> float
  val details : 'a t -> 'a
  val starred : [< project | action] t -> bool
  val action_state : [action] t -> [ `Next | `Waiting | `Future | `Done ]
  val project_state : [project] t -> [ `Active | `SomedayMaybe | `Done ]
end

module type EQ = sig
  type t
  val equal : t -> t -> bool
end

module type TREE_MODEL = sig
  module Sort_key : Slow_set.SORT_KEY

  module Item : sig
    (** The data part of a node (excluding the child nodes).
     * This is passed through. *)
    type generic
    val equal : generic -> generic -> bool
    val show : generic -> string
  end

  module Child_map : Map.S with type key = Sort_key.t
  (** Ordered list of child nodes. *)

  type move_data
  (** This is a hack to allow animations to correlate source and target widgets
   * for moves. *)

  type t
  type group_id
  val group_label : group_id -> string
  val item : t -> [ `Item of Ck_id.t * Item.generic | `Group of group_id ]
  val children : t -> t Child_map.t
end

module type REV = sig
  type t
  type +'a node
  module V : Irmin.VIEW with
    type key = string list and
    type value = string

  module Node : sig
    val rev : 'a node -> t

    include DISK_NODE
      with type 'a t = 'a node

    val uuid : [< area | project | action] t -> Ck_id.t
    val child_nodes : 'a t  -> generic M.t

    val key : _ t -> Sort_key.t
    (** A key for sorting by name. *)

    val equal : generic -> generic -> bool
    (** Note that the rev field is ignored, so nodes from different commits can
     * be equal. *)

    val equal_excl_children : generic -> generic -> bool

    val ty : [< action | project | area] t ->
      [ `Action of [> action] t
      | `Project of [> project] t
      | `Area of [> area] t ]
  end

  type commit = float * string

  val equal : t -> t -> bool

  val roots : t -> Node.generic M.t
  val history : t -> commit list  (* XXX: recent *)
  val make_view : t -> V.t Lwt.t

  val get : t -> Ck_id.t -> Node.generic option
  val parent : t -> Node.generic -> Node.generic option
end
