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

    type action = [`Action of action_node]
    type project = [`Project of project_node]
    type area = [`Area of area_node]
    type contact = [`Contact of contact_node]
  end
  open Types

  type generic = [ area | project | action | contact ]

  val parent : [< area | project | action ] -> Ck_id.t
  val name : [< generic ] -> string
  val description : [< generic] -> string
  val ctime : [< generic ] -> float
  val starred : [< project | action] -> bool
  val action_state : action_node -> [ `Next | `Waiting | `Waiting_for_contact of Ck_id.t | `Future | `Done  ]
  val project_state : project_node -> [ `Active | `SomedayMaybe | `Done ]
  val is_done : [< project | action] -> bool
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
  type group_id
  val group_label : group_id -> string
  val item : t -> [ `Item of Ck_id.t * Item.generic | `Group of group_id ]
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
    val rev : [< generic] -> t

    val uuid : [< generic ] -> Ck_id.t

    val key : [< generic ] -> Sort_key.t
    (** A key for sorting by name. *)

    val equal : generic -> generic -> bool
    (** Note that the rev field is ignored, so nodes from different commits can
     * be equal. *)
  end
  open Node.Types

  type commit

  val equal : t -> t -> bool
  val child_nodes : [< area | project | action ] -> [ area | project | action ] M.t

  val roots : t -> [ area | project | action ] M.t
  val commit : t -> commit

  val contacts : t -> contact_node Ck_id.M.t
  val actions_of : contact_node -> action_node list

  val get : t -> Ck_id.t -> [ area | project | action ] option
  val get_contact : t -> Ck_id.t -> contact_node option

  val parent : t -> [< area | project | action] -> [ area | project | action ] option
end
