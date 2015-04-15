(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

(** The data that gets stored to disk (e.g. parent UUID), but not data we calculate on loading
 * (e.g. list of children). *)

open Ck_sigs

type node_details
type action_details
type project_details

module Types : sig
  class type virtual node =
    object ('a)
      method details : node_details
      method name : string
      method description : string
      method ctime : float
      method conflicts : string list
      method with_conflict : string -> 'a
      method without_conflicts : 'a
      method map_details : (node_details -> node_details) -> 'a
      method virtual data : Obj.t
      method equals : node -> bool
      method virtual ty :
        [ `Area of area_node | `Project of project_node | `Action of action_node
        | `Contact of contact_node | `Context of context_node ]
    end
  and virtual contact_node = node
  and virtual context_node = node
  and virtual apa_node =
    object ('a)
      inherit node
      method virtual sexp : Sexplib.Sexp.t
      method parent : Ck_id.t
      method with_parent : Ck_id.t -> 'a
      method contact : Ck_id.t option
      method virtual apa_ty :
        [ `Area of area_node | `Project of project_node | `Action of action_node ]
    end
  and virtual area_node = apa_node
  and virtual project_node =
    object ('a)
      inherit apa_node
      method project : project_details
      method starred : bool
      method state : project_state
      method with_state : project_state -> 'a
      method with_starred : bool -> 'a
    end
  and virtual action_node =
    object ('a)
      inherit apa_node
      method action : action_details
      method starred : bool
      method state : action_state
      method context : Ck_id.t option
      method repeat : Ck_time.repeat option
      method with_repeat : Ck_time.repeat option -> 'a
      method with_state : action_state -> 'a
      method with_starred : bool -> 'a
      method with_context : Ck_id.t option -> 'a
    end

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
val action_state : action -> action_state
val action_repeat : action -> Ck_time.repeat option
val project_state : project -> project_state
val is_done : [< project | action] -> bool
val context : action -> Ck_id.t option
val contact : [< area | project | action ] -> Ck_id.t option

open Types

val of_string : string -> apa_node
val to_string : #apa_node -> string

val contact_of_string : string -> contact
val contact_to_string : contact -> string

val context_of_string : string -> context
val context_to_string : context -> string

val equal : ([< generic] as 'a) -> 'a -> bool

val make_action : state:action_state -> ?context:Ck_id.t -> ?contact:Ck_id.t -> name:string -> description:string -> parent:Ck_id.t -> ctime:float -> unit -> [> action]
val make_project : state:project_state -> ?contact:Ck_id.t -> name:string -> description:string -> parent:Ck_id.t -> ctime:float -> unit -> [> project]
val make_area : ?contact:Ck_id.t -> name:string -> description:string -> parent:Ck_id.t -> ctime:float -> unit -> [> area]
val make_contact : name:string -> description:string -> ctime:float -> unit -> contact
val make_context : name:string -> description:string -> ctime:float -> unit -> context

val with_name : generic -> string -> generic
val with_description : generic -> string -> generic
val with_parent : [< area | project | action] -> Ck_id.t -> [area | project | action]
val with_contact : [< area | project | action] -> Ck_id.t option -> [area | project | action]
val with_repeat : action -> Ck_time.repeat option -> [> action]
val with_astate : action -> action_state -> action
val with_pstate : project -> [ `Active | `SomedayMaybe | `Done ] -> project
val with_starred : [< project | action] -> bool -> [project | action]
val with_context : action -> Ck_id.t option -> action

val as_area : project -> area
val as_project : [< area | action] -> project
val as_action : project -> action

val merge : ?base:[< area | project | action] -> theirs:[< area | project | action] -> [< area | project | action] ->
  [area | project | action]
val merge_context : ?base:context -> theirs:context -> context -> context
val merge_contact : ?base:contact -> theirs:contact -> contact -> contact

val unwrap : [< generic] -> node
val unwrap_apa : [< `Area of area_node | `Project of project_node | `Action of action_node] -> apa_node
