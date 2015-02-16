(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Sexplib.Std
open Ck_utils
open Lwt

type uuid = string with sexp

let root_id : uuid = ""

let mint_uuid () = Uuidm.(create `V4 |> to_string)

type action_details = {
  astate : [ `Next | `Waiting | `Future ]
} with sexp

type project_details = {
  pstate : [ `Active | `SomedayMaybe ]
} with sexp

type action = [`Action of action_details]
type project = [`Project of project_details]
type area = [`Area]

type 'a node = {
  parent : uuid;
  name : string;
  description : string;
  details : 'a;
} with sexp

type general_node =
  [ `Action of action_details
  | `Project of project_details
  | `Area ] node
  with sexp

let root_node = {
  parent = root_id;
  name = "/";
  description = "Root area";
  details = `Area;
}

module Children = struct
  type t = (uuid, StringSet.t) Hashtbl.t

  let make () = Hashtbl.create 100

  let children t parent =
    try Hashtbl.find t parent
    with Not_found -> StringSet.empty

  let add t parent child =
    Hashtbl.replace t parent (children t parent |> StringSet.add child)
end

module Raw(I : Irmin.BASIC with type key = string list and type value = string) = struct
  type t = {
    store : string -> I.t;
    nodes : (uuid, general_node) Hashtbl.t;
    children : Children.t;
  }

  let make store =
    let nodes = Hashtbl.create 100 in
    let children = Children.make () in
    Hashtbl.add nodes root_id root_node;
    I.list (store "Find db nodes") ["db"] >>=
    Lwt_list.iter_s (function
      | ["db"; uuid] as key ->
          assert (uuid <> root_id);
          I.read_exn (store "Load db node") key >|= fun s ->
          let node = general_node_of_sexp (Sexplib.Sexp.of_string s) in
          Hashtbl.add nodes uuid node;
          let old_children =
            try Hashtbl.find children node.parent
            with Not_found -> StringSet.empty in
          Hashtbl.replace children node.parent (old_children |> StringSet.add uuid);
      | _ -> assert false
    ) >|= fun () ->
    children |> Hashtbl.iter (fun parent children ->
      if not (Hashtbl.mem nodes parent) then
        error "Parent UUID '%s' of child nodes %s missing!" parent (String.concat ", " (StringSet.elements children))
    );
    (* todo: reject cycles *)
    {store; nodes; children}

  let get t uuid =
    try Hashtbl.find t.nodes uuid
    with Not_found -> error "UUID '%s' not found in database!" uuid

  let create t uuid (node:[< action | project | area] node) =
    let node = (node :> general_node) in
    assert (uuid <> root_id);
    assert (not (Hashtbl.mem t.nodes uuid));
    if not (Hashtbl.mem t.nodes node.parent) then
      error "Parent '%s' does not exist!" node.parent;
    let s = Sexplib.Sexp.to_string (sexp_of_general_node node) in
    I.update (t.store "create") ["db"; uuid] s >|= fun () ->
    Children.add t.children node.parent uuid;
    Hashtbl.add t.nodes uuid node
end

module Make(I : Irmin.BASIC with type key = string list and type value = string) = struct
  module R = Raw(I)

  type t = R.t

  type 'a full_node = {
    t : t;
    id : uuid;
    raw : 'a node;
  }

  let children node =
    Children.children node.t.R.children node.id

  let make = R.make

  let root t = {t; id = root_id; raw = root_node}

  let all_areas_and_projects t =
    let results = ref [] in
    let rec scan x =
      results := x :: !results;
      children x |> StringSet.iter (fun child_id ->
        let child = R.get t child_id in
        match child with
        | {details = `Area | `Project _; _} as x -> scan {t; id = child_id; raw = x}
        | _ -> ()
      ) in
    scan (root t);
    List.rev !results

  let actions parent =
    let results = ref [] in
    children parent |> StringSet.iter (fun child_id ->
      let child = R.get parent.t child_id in
      match child with
      | {details = `Action _; _} as x -> results := {t = parent.t; raw = x; id = child_id} :: !results
      | _ -> ()
    );
    List.rev !results

  let projects parent =
    let results = ref [] in
    children parent |> StringSet.iter (fun child_id ->
      let child = R.get parent.t child_id in
      match child with
      | {details = `Project _; _} as x -> results := {t = parent.t; raw = x; id = child_id} :: !results
      | _ -> ()
    );
    List.rev !results

  let areas parent =
    let results = ref [] in
    children parent |> StringSet.iter (fun child_id ->
      let child = R.get parent.t child_id in
      match child with
      | {details = `Area; _} as x -> results := {t = parent.t; raw = x; id = child_id} :: !results
      | _ -> ()
    );
    List.rev !results

  let name node = node.raw.name

  let full_name node =
    if (node.raw :> general_node) == root_node then "(root)"
    else
      let t = node.t in
      let rec aux node =
        let p = R.get t node.parent in
        if p == root_node then [node.name]
        else node.name :: aux p in
      aux (node.raw :> general_node)
      |> List.rev
      |> String.concat "/"

  let uuid node = node.id

  let create node =
    R.create node.t node.id node.raw

  let add details t ~parent ~name ~description =
    let node = {
      t;
      id = mint_uuid ();
      raw = {
        name;
        description;
        parent;
        details;
      }
    } in
    create node

  let add_action = add (`Action {astate = `Next})
  let add_project = add (`Project {pstate = `Active})
  let add_area = add `Area
end
