(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Sexplib.Std
open Lwt

open Ck_utils

type uuid = string with sexp

let root_id : uuid = ""

let mint_uuid () = Uuidm.(create `V4 |> to_string)

module Disk_types = struct
  type action = [`Action of Ck_sigs.action_details]
  type project = [`Project of Ck_sigs.project_details]
  type area = [`Area]

  type 'a node = {
    parent : uuid;
    name : string;
    description : string;
    ctime : float with default(0.0);
    details : 'a;
  } with sexp

  type general_node =
    [ `Action of Ck_sigs.action_details
    | `Project of Ck_sigs.project_details
    | `Area ] node
    with sexp
end

let root_node = { Disk_types.
  parent = root_id;
  name = "All";
  description = "Root area";
  details = `Area;
  ctime = 0.0;
}

module type NODE = sig
  open Disk_types

  type node_set

  type 'a n = {
    uuid : uuid;
    disk_node : 'a node;
    child_nodes : node_set;
  }

  include Set.OrderedType
  with type t = [area | project | action] n
end

module Raw(I : Irmin.BASIC with type key = string list and type value = string) = struct
  open Disk_types

  module Top = Graph.Topological.Make(I.History)

  module rec Node : (NODE with type node_set = NodeSet.t) = struct
    type node_set = NodeSet.t

    type 'a n = {
      uuid : uuid;
      disk_node : 'a Disk_types.node;
      child_nodes : node_set;
    }

    type t = [area | project | action] n

    let compare a b =
      match String.compare a.disk_node.name b.disk_node.name with
      | 0 -> compare a.uuid b.uuid
      | r -> r
  end
  and NodeSet : (Set.S with type elt = Node.t) = Set.Make(Node)

  type t = {
    store : string -> I.t;
    root : 'a. ([> area] as 'a) Node.n;
    index : (uuid, Node.t) Hashtbl.t;
    history : (float * string) list;
  }

  let rec walk fn node =
    fn node;
    node.Node.child_nodes |> NodeSet.iter (walk fn)

  let make store =
    let disk_nodes = Hashtbl.create 100 in
    let children = Hashtbl.create 100 in
    Hashtbl.add disk_nodes root_id root_node;
    I.list (store "Find db nodes") ["db"] >>=
    Lwt_list.iter_s (function
      | ["db"; uuid] as key ->
          assert (uuid <> root_id);
          I.read_exn (store "Load db node") key >|= fun s ->
          let node = general_node_of_sexp (Sexplib.Sexp.of_string s) in
          Hashtbl.add disk_nodes uuid node;
          let old_children =
            try Hashtbl.find children node.parent
            with Not_found -> [] in
          Hashtbl.replace children node.parent (uuid :: old_children);
      | _ -> assert false
    ) >>= fun () ->
    children |> Hashtbl.iter (fun parent children ->
      if not (Hashtbl.mem disk_nodes parent) then (
        error "Parent UUID '%s' of child nodes %s missing!" parent (String.concat ", " children)
      )
    );

    (* todo: reject cycles *)
    let rec make_node uuid =
      let disk_node = Hashtbl.find disk_nodes uuid in { Node.
        uuid;
        disk_node;
        child_nodes = make_child_nodes uuid;
      }
    and make_child_nodes uuid =
      begin try Hashtbl.find children uuid with Not_found -> [] end
      |> List.map make_node
      |> List.fold_left (fun set node -> NodeSet.add node set) NodeSet.empty in

    let root = { Node.
      uuid = root_id;
      disk_node = root_node;
      child_nodes = make_child_nodes root_id;
    } in
    let index = Hashtbl.create 100 in
    root |> walk (fun node -> Hashtbl.add index node.Node.uuid node);
    I.history ~depth:10 (store "Read history") >>= fun history ->
    let h = ref [] in
    history |> Top.iter (fun head ->
      h := head :: !h
    );
    !h |> Lwt_list.map_s (fun hash ->
      I.task_of_head (store "Read commit") hash >|= fun task ->
      let summary =
        match Irmin.Task.messages task with
        | [] -> "(no commit message)"
        | x::_ -> x in
      let date = Irmin.Task.date task |> Int64.to_float in
      (date, summary)
    ) >|= fun history ->
    { store; root; index; history}

  let get t uuid =
    try Some (Hashtbl.find t.index uuid)
    with Not_found -> None

  let get_exn t uuid =
    try Hashtbl.find t.index uuid
    with Not_found -> error "UUID '%s' not found in database!" uuid

  (* Note: in theory, the result might not match the input type, if the
   * merge changes it for some reason. In practice, this shouldn't happen. *)
  let create t (node:[< action | project | area] node) =
    let node = (node :> general_node) in
    let uuid = mint_uuid () in
    assert (not (Hashtbl.mem t.index uuid));
    if not (Hashtbl.mem t.index node.parent) then
      error "Parent '%s' does not exist!" node.parent;
    let s = Sexplib.Sexp.to_string (sexp_of_general_node node) in
    let msg = Printf.sprintf "Create '%s'" node.name in
    I.update (t.store msg) ["db"; uuid] s >>= fun () ->
    make t.store >|= fun t_new ->
    (Hashtbl.find t_new.index uuid, t_new)

  let update t ~msg node =
    let open Node in
    let node = (node :> Node.t) in
    assert (Hashtbl.mem t.index node.uuid);
    if not (Hashtbl.mem t.index node.disk_node.parent) then
      error "Parent '%s' does not exist!" node.disk_node.parent;
    let s = Sexplib.Sexp.to_string (sexp_of_general_node node.disk_node) in
    I.update (t.store msg) ["db"; node.uuid] s >>= fun () ->
    make t.store

  let name n = n.Node.disk_node.name

  let delete t uuid =
    assert (uuid <> root_id);
    let node = get_exn t uuid in
    let msg = Printf.sprintf "Delete '%s'" (name node) in
    I.remove (t.store msg) ["db"; uuid] >>= fun () ->
    make t.store
end

module Make(I : Irmin.BASIC with type key = string list and type value = string) = struct
  module R = Raw(I)
  module NodeList = Delta_RList.Make(R.Node)(R.NodeSet)

  open R.Node

  type t = {
    current : R.t React.S.t;
    set_current : R.t -> unit;
  }

  type 'a full_node = 'a R.Node.n

  type area = Disk_types.area
  type project = Disk_types.project
  type action = Disk_types.action

  type node_view = {
    uuid : uuid;
    node_type : [ area | project | action | `Deleted ] React.S.t;
    ctime : float;
    name : string React.S.t;
    child_views : node_view ReactiveData.RList.t;
  }

  type details = {
    details_uuid : uuid;
    details_type : [ area | project | action | `Deleted ] React.S.t;
    details_name : string React.S.t;
    details_description : string React.S.t;
    details_children : node_view ReactiveData.RList.t;
  }

  let make store =
    R.make store >|= fun r ->
    let current, set_current = React.S.create r in
    { current; set_current }

  let root t = t.current |> React.S.map (fun r -> r.R.root)
  let is_root = (=) root_id

  let all_areas_and_projects t =
    let results = ref [] in
    let rec scan prefix x =
      let full_path = prefix ^ "/" ^ x.disk_node.Disk_types.name in
      results := (full_path, x) :: !results;
      x.child_nodes |> R.NodeSet.iter (fun child ->
        match child with
        | {disk_node = {Disk_types.details = `Area | `Project _; _}; _} as x -> scan full_path x
        | _ -> ()
      ) in
    scan "" (root t |> React.S.value);
    List.rev !results

  let actions parent =
    let results = ref [] in
    parent.child_nodes |> R.NodeSet.iter (fun child ->
      match child with
      | {disk_node = {Disk_types.details = `Action _; _}; _} as x -> results := x :: !results
      | _ -> ()
    );
    List.rev !results

  let projects parent =
    let results = ref [] in
    parent.child_nodes |> R.NodeSet.iter (fun child ->
      match child with
      | {disk_node = {Disk_types.details = `Project _; _}; _} as x -> results := x :: !results
      | _ -> ()
    );
    List.rev !results

  let areas parent =
    let results = ref [] in
    parent.child_nodes |> R.NodeSet.iter (fun child ->
      match child with
      | {disk_node = {Disk_types.details = `Area; _}; _} as x -> results := x :: !results
      | _ -> ()
    );
    List.rev !results

  let name node = node.disk_node.Disk_types.name

  let uuid node = node.R.Node.uuid

  let add details t ~parent ~name ~description =
    let disk_node = { Disk_types.
      name;
      description;
      parent;
      ctime = Unix.gettimeofday ();
      details;
    } in
    let r = React.S.value t.current in
    R.create r disk_node >|= fun (_node, r_new) ->
    t.set_current r_new

  let add_action = add (`Action {Ck_sigs.astate = `Next})
  let add_project = add (`Project {Ck_sigs.pstate = `Active})
  let add_area = add `Area

  let delete t uuid =
    let r = React.S.value t.current in
    R.delete r uuid >|= t.set_current

  let set_name t node name =
    let r = React.S.value t.current in
    let new_node = {node with
      disk_node = {node.disk_node with Disk_types.name}
    } in
    let msg = Printf.sprintf "Rename '%s' to '%s'" (R.name node) (R.name new_node) in
    R.update r ~msg new_node >|= t.set_current

  let set_state t uuid new_state =
    let r = React.S.value t.current in
    let node = R.get_exn r uuid in
    let new_node = {node with
      disk_node = {node.disk_node with Disk_types.details = new_state}
    } in
    let msg = Printf.sprintf "Change state of '%s'" (R.name node) in
    R.update r ~msg new_node >|= t.set_current

  let node_type {disk_node = {Disk_types.details; _}; _} = details
  let opt_node_type = function
    | None -> `Deleted
    | Some x -> (node_type x :> [action | project | area | `Deleted])
  let opt_node_name = function
    | None -> "(deleted)"
    | Some x -> R.name x
  let opt_node_description = function
    | None -> "(deleted)"
    | Some x -> x.disk_node.Disk_types.description
  let opt_child_nodes = function
    | None -> R.NodeSet.empty
    | Some x -> x.child_nodes

  let process_tree t =
    let rec view node =
      let live_node = t.current |> React.S.map (fun r -> R.get r node.R.Node.uuid) in
      let child_nodes = live_node |> React.S.map opt_child_nodes in
      let child_views = child_nodes
        |> NodeList.make ~init:(node.R.Node.child_nodes)
        |> ReactiveData.RList.map view in
      {
        uuid = node.R.Node.uuid;
        ctime = node.disk_node.Disk_types.ctime;
        node_type = live_node |> React.S.map opt_node_type;
        name = live_node |> React.S.map opt_node_name;
        child_views;
      } in
    let root_node = R.get_exn (React.S.value t.current) root_id in
    view root_node

  let collect_next_actions r =
    let results = ref R.NodeSet.empty in
    let rec scan = function
      | {disk_node = {Disk_types.details = `Area | `Project _; _}; _} as x ->
          results := actions x |> List.fold_left (fun set action ->
            match action with
            | {disk_node = {Disk_types.details = `Action {Ck_sigs.astate = `Next}; _}; _} ->
                R.NodeSet.add (action :> R.Node.t) set
            | _ -> set
          ) !results;
          x.child_nodes |> R.NodeSet.iter scan
      | {disk_node = {Disk_types.details = `Action _; _}; _} -> ()
    in
    scan r.R.root;
    !results

  let render_node t node =
    let live_node = t.current |> React.S.map (fun r -> R.get r node.R.Node.uuid) in
    {
      uuid = node.R.Node.uuid;
      ctime = node.R.Node.disk_node.Disk_types.ctime;
      node_type = live_node |> React.S.map opt_node_type;
      name = live_node |> React.S.map opt_node_name;
      child_views = ReactiveData.RList.empty;
    }

  let work_tree t =
    t.current
    |> React.S.map collect_next_actions
    |> NodeList.make
    |> ReactiveData.RList.map (render_node t)

  let details t uuid =
    let initial_node = R.get_exn (React.S.value t.current) uuid in
    let node = t.current |> React.S.map (fun r -> R.get r uuid) in
    let details_children = node
      |> React.S.map opt_child_nodes
      |> NodeList.make
      |> ReactiveData.RList.map (render_node t) in
    {
      details_uuid = initial_node.R.Node.uuid;
      details_type = node |> React.S.map opt_node_type;
      details_name = node |> React.S.map opt_node_name;
      details_description = node |> React.S.map opt_node_description;
      details_children;
    }

  let history t =
    t.current >|~= fun r -> r.R.history
end
