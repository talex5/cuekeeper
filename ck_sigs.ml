(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Sexplib.Std

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
  module Id_map : Map.S with type key = Ck_id.t
  (** Used to correlate nodes as the input updates. *)

  module Sort_key : Slow_set.SORT_KEY

  module Item : sig
    (** The data part of a node (excluding the child nodes).
     * This is passed through. *)
    type generic
    val equal : generic -> generic -> bool
    val show : generic -> string
  end

  module Child_map : Map.S with type key = Sort_key.t
  (** Ordered list of child nodes (probably not in the same order as Id_map). *)

  type move_data
  (** This is a hack to allow animations to correlate source and target widgets
   * for moves. *)

  type t
  val item : t -> Item.generic
  val id : t -> Ck_id.t
  val children : t -> t Child_map.t
end
