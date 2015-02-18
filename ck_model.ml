(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Sexplib.Std
open Lwt

open Ck_utils

type uuid = string with sexp

let root_id : uuid = ""

let mint_uuid () = Uuidm.(create `V4 |> to_string)

module Disk_types = struct
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
end

let root_node = { Disk_types.
  parent = root_id;
  name = "All";
  description = "Root area";
  details = `Area;
}

module NodeOrd = struct
  type t = string * uuid  (* Sort by name (using UUID only if identical) *)

  let compare (an, au) (bn, bu) =
    match String.compare an bn with
    | 0 -> compare au bu
    | r -> r
end

module NodeSet = struct
  include Set.Make(NodeOrd)
  let iter fn t =
    t |> iter (fun (_name, uuid) -> fn uuid)
end

module Children = struct
  type t = (uuid, NodeSet.t) Hashtbl.t

  let make () = Hashtbl.create 100

  let children t parent =
    try Hashtbl.find t parent
    with Not_found -> NodeSet.empty

  let add t parent child =
    Hashtbl.replace t parent (children t parent |> NodeSet.add child)
end

module Raw(I : Irmin.BASIC with type key = string list and type value = string) = struct
  open Disk_types

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
            with Not_found -> NodeSet.empty in
          Hashtbl.replace children node.parent (old_children |> NodeSet.add (node.name, uuid));
      | _ -> assert false
    ) >|= fun () ->
    children |> Hashtbl.iter (fun parent children ->
      if not (Hashtbl.mem nodes parent) then (
        let names = NodeSet.elements children |> List.map fst |> String.concat ", " in
        error "Parent UUID '%s' of child nodes %s missing!" parent names
      )
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
    Children.add t.children node.parent (node.name, uuid);
    Hashtbl.add t.nodes uuid node
end

module Make(I : Irmin.BASIC with type key = string list and type value = string) = struct
  module R = Raw(I)

  type t = R.t

  type 'a full_node = {
    t : t;
    id : uuid;
    raw : 'a Disk_types.node;
  }

  type project_details = Disk_types.project_details
  type action_details = Disk_types.action_details
  type area = Disk_types.area
  type project = Disk_types.project
  type action = Disk_types.action

  let children node =
    Children.children node.t.R.children node.id

  let make = R.make

  let root t = {t; id = root_id; raw = root_node}

  let all_areas_and_projects t =
    let results = ref [] in
    let rec scan x =
      results := x :: !results;
      children x |> NodeSet.iter (fun child_id ->
        let child = R.get t child_id in
        match child with
        | {Disk_types.details = `Area | `Project _; _} as x -> scan {t; id = child_id; raw = x}
        | _ -> ()
      ) in
    scan (root t);
    List.rev !results

  let actions parent =
    let results = ref [] in
    children parent |> NodeSet.iter (fun child_id ->
      let child = R.get parent.t child_id in
      match child with
      | {Disk_types.details = `Action _; _} as x -> results := {t = parent.t; raw = x; id = child_id} :: !results
      | _ -> ()
    );
    List.rev !results

  let projects parent =
    let results = ref [] in
    children parent |> NodeSet.iter (fun child_id ->
      let child = R.get parent.t child_id in
      match child with
      | {Disk_types.details = `Project _; _} as x -> results := {t = parent.t; raw = x; id = child_id} :: !results
      | _ -> ()
    );
    List.rev !results

  let areas parent =
    let results = ref [] in
    children parent |> NodeSet.iter (fun child_id ->
      let child = R.get parent.t child_id in
      match child with
      | {Disk_types.details = `Area; _} as x -> results := {t = parent.t; raw = x; id = child_id} :: !results
      | _ -> ()
    );
    List.rev !results

  let name node = node.raw.Disk_types.name

  let full_name node =
    if (node.raw :> Disk_types.general_node) == root_node then "(root)"
    else
      let t = node.t in
      let rec aux node =
        let p = R.get t node.Disk_types.parent in
        if p == root_node then [node.Disk_types.name]
        else node.Disk_types.name :: aux p in
      aux (node.raw :> Disk_types.general_node)
      |> List.rev
      |> String.concat "/"

  let uuid node = node.id

  let create node =
    R.create node.t node.id node.raw

  let add details t ~parent ~name ~description =
    let node = {
      t;
      id = mint_uuid ();
      raw = { Disk_types.
        name;
        description;
        parent = parent.id;
        details;
      }
    } in
    create node >>= fun () ->
    return node

  let add_action t = add (`Action {Disk_types.astate = `Next}) t
  let add_project t = add (`Project {Disk_types.pstate = `Active}) t
  let add_area t = add `Area t
end
