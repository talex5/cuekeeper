(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

(** The data that gets stored to disk (e.g. parent UUID), but not data we calculate on loading
 * (e.g. list of children). *)

type action_state = [ `Next | `Waiting | `Waiting_for_contact | `Waiting_until of Ck_time.user_date | `Future | `Done ]
type project_state = [ `Active | `SomedayMaybe | `Done ]

type node_details
type action_details
type project_details

module Types : sig
  class type virtual node =
    object ('a)
      method virtual dir : string
      method uuid : Ck_id.t
      method virtual sexp : Sexplib.Sexp.t
      method details : node_details
      method name : string
      method with_name : string -> 'a
      method description : string
      method with_description : string -> 'a
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
      method parent : Ck_id.t
      method with_parent : Ck_id.t -> 'a
      method contact : Ck_id.t option
      method with_contact : Ck_id.t option -> 'a
      method virtual apa_ty :
        [ `Area of area_node | `Project of project_node | `Action of action_node ]
      method virtual as_area : area_node
      method virtual as_project : project_node
      method virtual as_action : action_node
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

val apa_of_string : uuid:Ck_id.t -> string -> apa_node
val contact_of_string : uuid:Ck_id.t -> string -> contact_node
val context_of_string : uuid:Ck_id.t -> string -> context_node

val equal : #node -> #node -> bool

val make_action : state:action_state -> ?context:Ck_id.t -> ?contact:Ck_id.t -> name:string -> description:string -> parent:Ck_id.t -> ctime:float -> Ck_id.t -> action_node
val make_project : state:project_state -> ?contact:Ck_id.t -> name:string -> description:string -> parent:Ck_id.t -> ctime:float -> Ck_id.t -> project_node
val make_area : ?contact:Ck_id.t -> name:string -> description:string -> parent:Ck_id.t -> ctime:float -> Ck_id.t -> area_node
val make_contact : name:string -> description:string -> ctime:float -> Ck_id.t -> contact_node
val make_context : name:string -> description:string -> ctime:float -> Ck_id.t -> context_node

val merge_apa : ?base:apa_node -> theirs:apa_node -> apa_node -> apa_node
val merge_context : ?base:context_node -> theirs:context_node -> context_node -> context_node
val merge_contact : ?base:contact_node -> theirs:contact_node -> contact_node -> contact_node
