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

type astate =
  [ `Next
  | `Waiting
  | `Waiting_for_contact
  | `Waiting_until of Ck_time.user_date
  | `Future
  | `Done ] with sexp

type action_details = {
  astarred : bool with default(false);
  astate : astate;
  context : Ck_id.t sexp_option;
  repeat: Ck_time.repeat sexp_option;
} with sexp

type project_details = {
  pstarred : bool with default(false);
  pstate : [ `Active | `SomedayMaybe | `Done ]
} with sexp

type disk_apa =
  [ `Action of (action_details * node_details)
  | `Project of (project_details * node_details)
  | `Area of node_details ]
  with sexp

module Types = struct
  class virtual node details =
    object (self : 'a)
      val details = details
      method details = details
      method name = details.name
      method description = details.description
      method ctime = details.ctime
      method conflicts = details.conflicts
      method with_conflict conflict = {< details = {details with conflicts = conflict :: details.conflicts} >}
      method without_conflicts = {< details = {details with conflicts = []} >}
      method map_details fn = {< details = fn details >}
      method virtual data : Obj.t
      method equals (other:node) = self#data = other#data
      method virtual ty :
        [ `Area of area_node | `Project of project_node | `Action of action_node
        | `Contact of contact_node | `Context of context_node ]
    end
  and virtual contact_node = node
  and virtual context_node = node
  and virtual apa_node details =
    object (self)
      inherit node details
      method virtual sexp : Sexplib.Sexp.t
      method parent = details.parent
      method with_parent parent = {< details = {details with parent} >}
      method contact = details.contact
      method virtual apa_ty :
        [ `Area of area_node | `Project of project_node | `Action of action_node ]
      method ty = (self#apa_ty :> [ `Area of area_node | `Project of project_node | `Action of action_node
                                  | `Contact of contact_node | `Context of context_node ])

    end
  and virtual area_node = apa_node
  and virtual project_node project_details details =
    object
      inherit apa_node details
      val project_details = project_details
      method project = project_details
      method starred = project_details.pstarred
      method state = project_details.pstate
      method with_state pstate = {< project_details = {project_details with pstate} >}
      method with_starred s = {< project_details = {project_details with pstarred = s} >}
    end
  and virtual action_node action_details details =
    object
      inherit apa_node details
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

let contact_node details =
  object (self : #contact_node)
    inherit node details
    method ty = `Contact self
    method data = Obj.repr details
  end

let context_node details =
  object (self : #context_node)
    inherit node details
    method ty = `Context self
    method data = Obj.repr details
  end

let area_node details =
  object (self : #area_node)
    inherit apa_node details
    method apa_ty = `Area self
    method sexp = sexp_of_disk_apa (`Area details :> disk_apa)
    method data = Obj.repr details
  end

let project_node project_details details =
  object (self : #project_node)
    inherit project_node project_details details
    method apa_ty = `Project self
    method sexp = sexp_of_disk_apa (`Project (project_details, details) :> disk_apa)
    method data = Obj.repr (project_details, details)
  end

let action_node action_details details =
  object (self : #action_node)
    inherit action_node action_details details
    method apa_ty = `Action self
    method sexp = sexp_of_disk_apa (`Action (action_details, details) :> disk_apa)
    method data = Obj.repr (action_details, details)
  end

type apa =
  [ `Action of action_node
  | `Project of project_node
  | `Area of area_node ]

type generic =
  [ apa
  | `Contact of contact_node
  | `Context of context_node ]

let details : [< generic] -> node_details = function
  | `Action d -> d#details
  | `Project d -> d#details
  | `Area d -> d#details
  | `Contact d -> d#details
  | `Context d -> d#details

let unwrap_apa : [< apa] -> apa_node = function
  | `Action n -> (n :> apa_node)
  | `Project n -> (n :> apa_node)
  | `Area n -> (n :> apa_node)

let unwrap : [< generic] -> node = function
  | `Action n -> (n :> node)
  | `Project n -> (n :> node)
  | `Area n -> (n :> node)
  | `Contact n -> (n :> node)
  | `Context n -> (n :> node)

let ctime t = (details t).ctime
let name t = (details t).name
let description t = (details t).description
let parent t = (unwrap_apa t)#parent
let contact t = (details t).contact
let conflicts t = (details t).conflicts

let of_string s =
  match disk_apa_of_sexp (Sexplib.Sexp.of_string s) with
  | `Action (a, d) -> (action_node a d :> apa_node)
  | `Project (p, d) -> (project_node p d :> apa_node)
  | `Area d -> (area_node d :> apa_node)
let to_string t =
  Sexplib.Sexp.to_string t#sexp

let contact_of_string s = `Contact (contact_node (node_details_of_sexp (Sexplib.Sexp.of_string s)))
let contact_to_string (`Contact t) = Sexplib.Sexp.to_string (sexp_of_node_details t#details)

let context_of_string s = `Context (context_node (node_details_of_sexp (Sexplib.Sexp.of_string s)))
let context_to_string (`Context t) = Sexplib.Sexp.to_string (sexp_of_node_details t#details)

let make ~name ~description ~parent ~ctime ~contact = {
  name;
  description;
  parent;
  ctime;
  contact;
  conflicts = [];
}

let with_name node name = ((unwrap node)#map_details (fun d -> {d with name}))#ty
let with_description node description = ((unwrap node)#map_details (fun d -> {d with description}))#ty
let with_parent node parent = ((unwrap_apa node)#map_details (fun d -> {d with parent}))#apa_ty
let with_contact node contact = ((unwrap_apa node)#map_details (fun d -> {d with contact}))#apa_ty
let equal a b =
  (unwrap (a : [< generic] :> generic))#equals (unwrap (b : [< generic] :> generic))

let context (`Action a) = a#context
let action_repeat (`Action a) = a#repeat
let action_state (`Action a) = a#state
let project_state (`Project p) = p#state
let starred = function
  | `Project n -> n#starred
  | `Action n -> n#starred

let with_repeat (`Action a) repeat = `Action (a#with_repeat repeat)
let with_astate (`Action a) state = `Action (a#with_state state)
let with_pstate (`Project p) state = `Project (p#with_state state)

let with_starred node s =
  match node with
  | `Action a -> `Action (a#with_starred s)
  | `Project p -> `Project (p#with_starred s)

let with_context (`Action a) context = `Action (a#with_context context)

let make_action ~state ?context ?contact ~name ~description ~parent ~ctime () =
  `Action (action_node { astate = state; astarred = false; context; repeat = None } (make ~name ~description ~parent ~ctime ~contact))

let make_project ~state ?contact ~name ~description ~parent ~ctime () =
  `Project (project_node { pstate = state; pstarred = false } (make ~name ~description ~parent ~ctime ~contact))

let make_area ?contact ~name ~description ~parent ~ctime () =
  `Area (area_node (make ~name ~description ~parent ~ctime ~contact))

let make_contact ~name ~description ~ctime () =
  `Contact (contact_node (make ~name ~description ~parent:Ck_id.root ~ctime ~contact:None))

let make_context ~name ~description ~ctime () =
  `Context (context_node (make ~name ~description ~parent:Ck_id.root ~ctime ~contact:None))

let is_done = function
  | `Action n -> n#state = `Done
  | `Project n -> n#state = `Done

let as_project = function
  | `Action n -> `Project (project_node {pstate = `Active; pstarred = n#starred} n#details)
  | `Project _ as p -> p
  | `Area n -> `Project (project_node {pstate = `Active; pstarred = false} n#details)

let as_area = function
  | `Area _ as a -> a
  | `Project n -> `Area (area_node n#details)
  | `Action n -> `Area (area_node n#details)

let as_action = function
  | `Project p -> `Action (action_node {astate = `Next; astarred = p#starred; context = None; repeat = None} p#details)
  | `Action _ as a -> a
  | `Area d -> `Action (action_node {astate = `Next; astarred = false; context = None; repeat = None} d#details)

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
  let `Project base = base in
  let {pstarred; pstate} = ours#project in
  let prj = {
    pstarred = merge_detail ~log ~fmt:star ~base:base#starred ~theirs:theirs#starred pstarred;
    pstate = merge_detail ~log ~fmt:fmt_pstate ~base:base#state ~theirs:theirs#state pstate;
  } in
  project_node prj (merge_details ~log ~base:base#details ~theirs:theirs#details ours#details)

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
  let `Action base = base in
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
  action_node act details

let merge ?base ~theirs ours =
  let base = (base :> apa option) |> default (`Area (area_node default_base)) in
  let theirs = (theirs :> apa) in
  let ours = (ours :> apa) in
  if equal base theirs then ours
  else if equal base ours then theirs
  else (
    let conflicts = ref [] in
    let log msg = conflicts := msg :: !conflicts in
    let merged =
      match theirs, ours with
      | `Area theirs, `Area ours -> area_node (merge_details ~log ~base:(details base) ~theirs:theirs#details ours#details)
      | `Project theirs, `Project ours -> (merge_project ~log ~base:(as_project base) ~theirs ours :> apa_node)
      | `Action theirs, `Action ours -> (merge_action ~log ~base:(as_action base) ~theirs ours :> apa_node)
      | theirs, ours ->
          log "Type mismatch: converting to project";
          let `Project theirs = as_project theirs in
          let `Project ours = as_project ours in
          (merge_project ~log ~base:(as_project base) ~theirs ours :> apa_node)
    in
    (merged#map_details (fun d -> {d with conflicts = d.conflicts @ !conflicts}))#apa_ty
  )

let merge_context ?base ~theirs ours =
  let base = base |> default (`Context (context_node default_base)) in
  if equal base theirs then ours
  else if equal base ours then theirs
  else (
    let `Context base = base in
    let `Context theirs = theirs in
    let `Context ours = ours in
    let conflicts = ref [] in
    let log msg = conflicts := msg :: !conflicts in
    let merged = merge_details ~log ~base:base#details ~theirs:theirs#details ours#details in
    `Context (context_node {merged with conflicts = merged.conflicts @ !conflicts})
  )

let merge_contact ?base ~theirs ours =
  let base = base |> default (`Contact (contact_node default_base)) in
  if equal base theirs then ours
  else if equal base ours then theirs
  else (
    let `Contact base = base in
    let `Contact theirs = theirs in
    let `Contact ours = ours in
    let conflicts = ref [] in
    let log msg = conflicts := msg :: !conflicts in
    let merged = merge_details ~log ~base:base#details ~theirs:theirs#details ours#details in
    `Contact (contact_node {merged with conflicts = merged.conflicts @ !conflicts})
  )
