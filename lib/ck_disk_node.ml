(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Sexplib.Conv
open Ck_utils

type node_details = {
  parent : Ck_id.t;
  name : string;
  description : string;
  ctime : float;
  contact : Ck_id.t sexp_option;
  conflicts : string sexp_list;
} with sexp

type action_state =
  [ `Next
  | `Waiting
  | `Waiting_for_contact
  | `Waiting_until of Ck_time.user_date
  | `Future
  | `Done ] with sexp

type project_state =
  [ `Active | `SomedayMaybe | `Done ]
  with sexp

type action_details = {
  astarred : bool with default(false);
  astate : action_state;
  context : Ck_id.t sexp_option;
  repeat: Ck_time.repeat sexp_option;
} with sexp

type project_details = {
  pstarred : bool with default(false);
  pstate : project_state
} with sexp

type disk_apa =
  [ `Action of (action_details * node_details)
  | `Project of (project_details * node_details)
  | `Area of node_details ]
  with sexp

module Types = struct
  class virtual node uuid details =
    object (self : 'a)
      val details = details
      method virtual dir : string
      method uuid : Ck_id.t = uuid
      method virtual sexp : Sexplib.Sexp.t
      method details = details
      method name = details.name
      method with_name name = {< details = {details with name} >}
      method description = details.description
      method with_description description = {< details = {details with description} >}
      method ctime = details.ctime
      method conflicts = details.conflicts
      method with_conflict conflict = {< details = {details with conflicts = conflict :: details.conflicts} >}
      method without_conflicts = {< details = {details with conflicts = []} >}
      method map_details fn = {< details = fn details >}
      method virtual data : Obj.t
      method equals (other:node) = uuid = other#uuid && self#data = other#data
      method virtual ty :
        [ `Area of area_node | `Project of project_node | `Action of action_node
        | `Contact of contact_node | `Context of context_node ]
    end
  and virtual contact_node uuid details =
    object
      inherit node uuid details
      method dir = "contact"
    end
  and virtual context_node uuid details =
    object
      inherit node uuid details
      method dir = "context"
    end
  and virtual apa_node uuid details =
    object (self)
      inherit node uuid details
      method dir = "db"
      method parent = details.parent
      method with_parent parent = {< details = {details with parent} >}
      method contact = details.contact
      method with_contact contact = {< details = {details with contact} >}
      method virtual apa_ty :
        [ `Area of area_node | `Project of project_node | `Action of action_node ]
      method ty = (self#apa_ty :> [ `Area of area_node | `Project of project_node | `Action of action_node
                                  | `Contact of contact_node | `Context of context_node ])
      method virtual as_area : area_node
      method virtual as_project : project_node
      method virtual as_action : action_node
    end
  and virtual area_node = apa_node
  and virtual project_node uuid project_details details =
    object
      inherit apa_node uuid details
      val project_details = project_details
      method project = project_details
      method starred = project_details.pstarred
      method state = project_details.pstate
      method with_state pstate = {< project_details = {project_details with pstate} >}
      method with_starred s = {< project_details = {project_details with pstarred = s} >}
    end
  and virtual action_node uuid action_details details =
    object
      inherit apa_node uuid details
      val action_details = action_details
      method action = action_details
      method starred = action_details.astarred
      method state = action_details.astate
      method context = action_details.context
      method repeat = action_details.repeat
      method with_repeat repeat = {< action_details = {action_details with repeat} >}
      method with_state astate = {< action_details = {action_details with astate} >}
      method with_starred astarred = {< action_details = {action_details with astarred} >}
      method with_context context = {< action_details = {action_details with context} >}
    end

  type action = [`Action of action_node]
  type project = [`Project of project_node]
  type area = [`Area of area_node]
  type contact = [`Contact of contact_node]
  type context = [`Context of context_node]
end
open Types

let contact_node uuid details =
  object (self : #contact_node)
    inherit contact_node uuid details
    method ty = `Contact self
    method data = Obj.repr details
    method sexp = sexp_of_node_details details
  end

let context_node uuid details =
  object (self : #context_node)
    inherit context_node uuid details
    method ty = `Context self
    method data = Obj.repr details
    method sexp = sexp_of_node_details details
  end

let rec area_node uuid details =
  object (self : #Types.area_node)
    inherit apa_node uuid details
    method apa_ty = `Area self
    method sexp = sexp_of_disk_apa (`Area details :> disk_apa)
    method data = Obj.repr details
    method as_area = self
    method as_project = project_node uuid {pstate = `Active; pstarred = false} details
    method as_action = action_node uuid {astate = `Next; astarred = false; context = None; repeat = None} details
  end
and project_node uuid project_details details =
  object (self : #Types.project_node)
    inherit project_node uuid project_details details
    method apa_ty = `Project self
    method sexp = sexp_of_disk_apa (`Project (project_details, details) :> disk_apa)
    method data = Obj.repr (project_details, details)
    method as_area = area_node uuid details
    method as_project = self
    method as_action = action_node uuid {astate = `Next; astarred = project_details.pstarred; context = None; repeat = None} details
  end
and action_node uuid action_details details =
  object (self : #Types.action_node)
    inherit action_node uuid action_details details
    method apa_ty = `Action self
    method sexp = sexp_of_disk_apa (`Action (action_details, details) :> disk_apa)
    method data = Obj.repr (action_details, details)
    method as_area = area_node uuid details
    method as_project = project_node uuid {pstate = `Active; pstarred = action_details.astarred} details
    method as_action = self
  end

let apa_of_string ~uuid s =
  match disk_apa_of_sexp (Sexplib.Sexp.of_string s) with
  | `Action (a, d) -> (action_node uuid a d :> apa_node)
  | `Project (p, d) -> (project_node uuid p d :> apa_node)
  | `Area d -> (area_node uuid d :> apa_node)

let contact_of_string ~uuid s = contact_node uuid (node_details_of_sexp (Sexplib.Sexp.of_string s))
let context_of_string ~uuid s = context_node uuid (node_details_of_sexp (Sexplib.Sexp.of_string s))

let make ~name ~description ~parent ~ctime ~contact = {
  name;
  description;
  parent;
  ctime;
  contact;
  conflicts = [];
}

let equal a b =
  (a :> node)#equals (b :> node)

let make_action ~state ?context ?contact ~name ~description ~parent ~ctime uuid =
  action_node uuid { astate = state; astarred = false; context; repeat = None } (make ~name ~description ~parent ~ctime ~contact)

let make_project ~state ?contact ~name ~description ~parent ~ctime uuid =
  project_node uuid { pstate = state; pstarred = false } (make ~name ~description ~parent ~ctime ~contact)

let make_area ?contact ~name ~description ~parent ~ctime uuid =
  area_node uuid (make ~name ~description ~parent ~ctime ~contact)

let make_contact ~name ~description ~ctime uuid =
  contact_node uuid (make ~name ~description ~parent:Ck_id.root ~ctime ~contact:None)

let make_context ~name ~description ~ctime uuid =
  context_node uuid (make ~name ~description ~parent:Ck_id.root ~ctime ~contact:None)

let merge_detail ~log ~fmt ~base ~theirs ours =
  if base = theirs then ours
  else if base = ours then theirs
  else if theirs = ours then theirs
  else (
    let keep, discard =
      if ours < theirs then ours, theirs
      else theirs, ours in
    Printf.sprintf "Discarded change %s -> %s" (fmt base) (fmt discard) |> log;
    keep
  )

(* Used for the (unlikely) case of a merge with no common ancestor *)
let default_base = make ~name:"" ~description:"" ~parent:Ck_id.root ~ctime:0.0 ~contact:None

let opt_uuid = function
  | None -> "(none)"
  | Some uuid -> Ck_id.to_string uuid
let fmt_repeat = function
  | None -> "never"
  | Some repeat -> Ck_time.string_of_repeat repeat
let str x = x
let star = function
  | false -> "unstarred"
  | true -> "starred"
let fmt_pstate = function
  | `Active -> "active"
  | `Done -> "done"
  | `SomedayMaybe -> "someday/maybe"
let fmt_astate = function
  | `Next -> "next"
  | `Waiting -> "waiting"
  | `Waiting_for_contact -> "waiting for contact"
  | `Waiting_until date -> Printf.sprintf "waiting until %s" (Ck_time.string_of_user_date date)
  | `Future -> "future"
  | `Done -> "done"

let dedup xs =
  let rec aux acc = function
    | [] -> acc
    | x :: ((y :: _) as rest) when x = y -> aux acc rest
    | x :: xs -> aux (x :: acc) xs in
  aux [] (List.sort String.compare xs)

let merge_details ~log ~base ~theirs ours =
  let {parent; name; description; ctime; contact; conflicts} = ours in
  let parent      = merge_detail ~log ~fmt:Ck_id.to_string ~base:base.parent ~theirs:theirs.parent parent in
  let name        = merge_detail ~log ~fmt:str ~base:base.name ~theirs:theirs.name name in
  let description = merge_detail ~log ~fmt:str ~base:base.description ~theirs:theirs.description description in
  let ctime       = min (min base.ctime theirs.ctime) ctime in
  let contact     = merge_detail ~log ~fmt:opt_uuid ~base:base.contact ~theirs:theirs.contact contact in
  let conflicts   = dedup (conflicts @ theirs.conflicts) in
  {parent; name; description; ctime; contact; conflicts}

let merge_project ~log ~base ~theirs ours =
  let {pstarred; pstate} = ours#project in
  let prj = {
    pstarred = merge_detail ~log ~fmt:star ~base:base#starred ~theirs:theirs#starred pstarred;
    pstate = merge_detail ~log ~fmt:fmt_pstate ~base:base#state ~theirs:theirs#state pstate;
  } in
  project_node ours#uuid prj (merge_details ~log ~base:base#details ~theirs:theirs#details ours#details)

(* If we decided the final state should be [`Waiting_for_contact] then make sure we pick the
 * contact from the same place. *)
let merge_waiting_contact ~log ~theirs ours =
  if theirs#contact = ours#contact then theirs#details, ours#details
  else match theirs#state, ours#state with
  | `Waiting_for_contact, `Waiting_for_contact -> theirs#details, ours#details  (* Normal merge *)
  | `Waiting_for_contact, _ ->
      log "Different contacts; picking the one we were waiting for";
      theirs#details, {ours#details with contact = theirs#details.contact}
  | _, `Waiting_for_contact ->
      log "Different contacts; picking the one we were waiting for";
      {theirs#details with contact = ours#details.contact}, ours#details
  | _ -> assert false

let merge_action ~log ~base ~theirs ours =
  let {astarred; astate; context; repeat} = ours#action in
  let repeat = merge_detail ~log ~fmt:fmt_repeat ~base:base#repeat ~theirs:theirs#repeat repeat in
  let astate = merge_detail ~log ~fmt:fmt_astate ~base:base#state ~theirs:theirs#state astate in
  let astate =
    match astate with
    | `Done when repeat <> None -> log "Set to repeat and marked done"; `Next
    | s -> s in
  let details =
    let their_details, our_details =
      match astate with
      | `Waiting_for_contact -> merge_waiting_contact ~log ~theirs ours
      | _ -> theirs#details, ours#details in
    merge_details ~log ~base:base#details ~theirs:their_details our_details in
  let act = {
    astarred = merge_detail ~log ~fmt:star ~base:base#starred ~theirs:theirs#starred astarred;
    astate;
    context = merge_detail ~log ~fmt:opt_uuid ~base:base#context ~theirs:theirs#context context;
    repeat;
  } in
  action_node ours#uuid act details

let merge_apa ?base ~theirs ours =
  let uuid = ours#uuid in
  let base = base |> default (area_node uuid default_base) in
  if equal base theirs then ours
  else if equal base ours then theirs
  else (
    let conflicts = ref [] in
    let log msg = conflicts := msg :: !conflicts in
    let merged =
      match theirs#apa_ty, ours#apa_ty with
      | `Area theirs, `Area ours -> area_node uuid (merge_details ~log ~base:base#details ~theirs:theirs#details ours#details)
      | `Project theirs, `Project ours -> (merge_project ~log ~base:base#as_project ~theirs ours :> apa_node)
      | `Action theirs, `Action ours -> (merge_action ~log ~base:base#as_action ~theirs ours :> apa_node)
      | _ ->
          log "Type mismatch: converting to project";
          (merge_project ~log ~base:base#as_project ~theirs:theirs#as_project ours#as_project :> apa_node)
    in
    merged#map_details (fun d -> {d with conflicts = d.conflicts @ !conflicts})
  )

let merge_context ?base ~theirs ours =
  let uuid = ours#uuid in
  let base = base |> default (context_node uuid default_base) in
  if equal base theirs then ours
  else if equal base ours then theirs
  else (
    let conflicts = ref [] in
    let log msg = conflicts := msg :: !conflicts in
    let merged = merge_details ~log ~base:base#details ~theirs:theirs#details ours#details in
    context_node uuid {merged with conflicts = merged.conflicts @ !conflicts}
  )

let merge_contact ?base ~theirs ours =
  let uuid = ours#uuid in
  let base = base |> default (contact_node uuid default_base) in
  if equal base theirs then ours
  else if equal base ours then theirs
  else (
    let conflicts = ref [] in
    let log msg = conflicts := msg :: !conflicts in
    let merged = merge_details ~log ~base:base#details ~theirs:theirs#details ours#details in
    contact_node uuid {merged with conflicts = merged.conflicts @ !conflicts}
  )
