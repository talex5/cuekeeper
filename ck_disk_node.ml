(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Sexplib.Conv

type node_details = {
  parent : Ck_id.t;
  name : string;
  description : string;
  ctime : float;
  contact : Ck_id.t sexp_option;
} with sexp

type astate =
  [ `Next
  | `Waiting
  | `Waiting_for_contact
  | `Waiting_until of float
  | `Future
  | `Done ] with sexp

type action_details = {
  astarred : bool with default(false);
  astate : astate;
  context : Ck_id.t sexp_option;
} with sexp

type project_details = {
  pstarred : bool with default(false);
  pstate : [ `Active | `SomedayMaybe | `Done ]
} with sexp

type apa =
  [ `Action of (action_details * node_details)
  | `Project of (project_details * node_details)
  | `Area of node_details ]
  with sexp

type generic =
  [ apa
  | `Contact of node_details
  | `Context of node_details ]

module Types = struct
  type action_node = action_details * node_details
  type project_node = project_details * node_details
  type area_node = node_details
  type contact_node = node_details
  type context_node = node_details

  type action = [`Action of action_node]
  type project = [`Project of project_node]
  type area = [`Area of area_node]
  type contact = [`Contact of contact_node]
  type context = [`Context of context_node]
end

let details = function
  | `Action (_, d)
  | `Project (_, d)
  | `Area d -> d
  | `Contact d -> d
  | `Context d -> d

let ctime t = (details t).ctime
let name t = (details t).name
let description t = (details t).description
let parent t = (details t).parent
let contact t = (details t).contact

let of_string s = apa_of_sexp (Sexplib.Sexp.of_string s)
let to_string t = Sexplib.Sexp.to_string (sexp_of_apa (t :> apa))

let contact_of_string s = node_details_of_sexp (Sexplib.Sexp.of_string s)
let contact_to_string t = Sexplib.Sexp.to_string (sexp_of_node_details t)

let context_of_string s = node_details_of_sexp (Sexplib.Sexp.of_string s)
let context_to_string t = Sexplib.Sexp.to_string (sexp_of_node_details t)

let make ~name ~description ~parent ~ctime = {
  name;
  description;
  parent;
  ctime;
  contact = None;
}

let map_apa fn = function
  | `Action (x, d) -> `Action (x, fn d)
  | `Project (x, d) -> `Project (x, fn d)
  | `Area d -> `Area (fn d)

let map_details fn = function
  | `Action _ | `Project _ | `Area _ as node -> map_apa fn node
  | `Contact d -> `Contact (fn d)
  | `Context d -> `Context (fn d)

let with_name node name = node |> map_details (fun d -> {d with name})
let with_description node description = node |> map_details (fun d -> {d with description})
let with_parent node parent = node |> map_apa (fun d -> {d with parent})
let with_contact node contact = node |> map_apa (fun d -> {d with contact})
let equal = (=)

let context (action_details, _) = action_details.context
let action_state ({ astate; _ }, _) = astate
let project_state ({ pstate; _ }, _) = pstate
let starred = function
  | `Project ({ pstarred; _ }, _ ) -> pstarred
  | `Action ({ astarred; _ }, _) -> astarred

let with_astate (a, details) astate = ({a with astate}, details)
let with_pstate (p, details) pstate = ({p with pstate}, details)

let with_starred node s =
  match node with
  | `Action (a, d) -> `Action ({a with astarred = s}, d)
  | `Project (p, d) -> `Project ({p with pstarred = s}, d)

let with_context (a, details) context = ({a with context}, details)

let make_action ~state ?context ~name ~description ~parent ~ctime =
  `Action ({ astate = state; astarred = false; context }, make ~name ~description ~parent ~ctime)

let make_project ~name ~description ~parent ~ctime =
  `Project ({ pstate = `Active; pstarred = false }, make ~name ~description ~parent ~ctime)

let make_area ~name ~description ~parent ~ctime =
  `Area (make ~name ~description ~parent ~ctime)

let make_contact ~name ~description ~ctime =
  make ~name ~description ~parent:Ck_id.root ~ctime

let make_context ~name ~description ~ctime =
  make ~name ~description ~parent:Ck_id.root ~ctime

let is_done = function
  | `Action ({ astate; _}, _) -> astate = `Done
  | `Project ({ pstate; _}, _) -> pstate = `Done

let as_project = function
  | `Action ({ astarred; _}, d) -> ({pstate = `Active; pstarred = astarred}, d)
  | `Area d -> ({pstate = `Active; pstarred = false}, d)

let as_area (_, d) = d

let as_action ({ pstarred; _}, d) = ({astate = `Next; astarred = pstarred; context = None}, d)
