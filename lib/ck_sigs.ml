(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Ck_utils

type stop = unit -> unit
type 'a or_error = [ `Ok of 'a | `Error of string ]

module type DISK_NODE = sig
  module Types : sig
    type action_node
    type project_node
    type area_node
    type contact_node
    type context_node

    type action = [`Action of action_node]
    type project = [`Project of project_node]
    type area = [`Area of area_node]
    type contact = [`Contact of contact_node]
    type context = [`Context of context_node]
  end
  open Types

  type generic = [ area | project | action | contact | context ]

  val parent : [< area | project | action ] -> Ck_id.t
  val name : [< generic ] -> string
  val description : [< generic] -> string
  val ctime : [< generic ] -> float
  val conflicts : [< generic ] -> string list
  val starred : [< project | action] -> bool
  val action_state : action -> Ck_disk_node.action_state
  val action_repeat : action -> Ck_time.repeat option
  val project_state : project -> Ck_disk_node.project_state
  val is_done : [< project | action] -> bool
  val context : action -> Ck_id.t option
  val contact : [< area | project | action ] -> Ck_id.t option
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

  type t
  type group
  type adder
  val group_label : group -> string
  val adder : t -> adder option
  val item : t ->
    [ `UniqueItem of Ck_id.t * Item.generic   (* ID is unique in tree *)
    | `GroupItem of Ck_id.t * Item.generic    (* ID is unique within parent *)
    | `Group of group ]                       (* Label is unique within parent *)
  val children : t -> t Child_map.t
end

module type GUI_DATA = sig
  type t
  (** For extra data the GUI wants to attach to tree nodes. *)
end

module type REV = sig
  type t

  module Node : sig
    include DISK_NODE
    open Types

    type 'a node_details

    val rev : [< generic] -> t

    val uuid : [< generic ] -> Ck_id.t

    val key : [< generic ] -> Sort_key.t
    (** A key for sorting by name. *)

    val equal : [< generic] -> [< generic] -> bool
    (** Note that the rev field is ignored, so nodes from different commits can
     * be equal. *)

    val is_due : Types.action -> bool
    (** [true] if this is a waiting action due at or before the time
     * this revision was loaded. *)

    val apa_ty : #Ck_disk_node.Types.apa_node node_details -> [area | project | action]
  end
  open Node.Types

  type commit

  val equal : t -> t -> bool
  val child_nodes : [< area | project | action ] -> [ area | project | action ] M.t

  val roots : t -> [ area | project | action ] M.t
  val commit : t -> commit

  val nodes : t -> [ area | project | action] Ck_id.M.t

  val contacts : t -> contact Ck_id.M.t
  val nodes_of_contact : contact -> [ area | project | action ] list
  val contact_for : [< area | project | action ] -> contact option

  val contexts : t -> context Ck_id.M.t
  val actions_of_context : context -> action list

  val get : t -> Ck_id.t -> [ area | project | action ] option
  val get_contact : t -> Ck_id.t -> [> contact] option
  val get_context : t -> Ck_id.t -> [> context] option

  val parent : t -> [< area | project | action] -> [ area | project | action ] option
  val context : action -> context option

  val schedule : t -> action list
  (** The ([`Waiting_until time] actions, earliest first. *)

  val problems : t -> (Node.generic * string) list
  (** A list of nodes and problems to report. *)

  val alert : t -> bool
  (** Alert the user that action is required.
   * Currently, this is true when a [`Waiting_until] action is due, or
   * [problems t] is non-empty. *)

  val expires : t -> Ck_time.user_date option
  (** Will need to reload at this time (this is when the next scheduled action becomes due). *)
end
