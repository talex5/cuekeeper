(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Sexplib.Std
open Lwt

open Ck_utils
open Ck_sigs

module Node = Ck_node
module M = Ck_node.M

module Raw(I : Irmin.BASIC with type key = string list and type value = string) = struct
  module Top = Graph.Topological.Make(I.History)

  type t = {
    store : string -> I.t;
    commit : I.head;
    root : 'a. ([> area] as 'a) Node.t;
    index : (Ck_id.t, Node.generic) Hashtbl.t;
    history : (float * string) list;
  }

  let eq a b =
    a.commit = b.commit

  let rec walk fn node =
    fn node;
    node.Node.child_nodes |> M.iter (fun _k v -> walk fn v)

  let get_current store =
    I.head (store "Get latest commit") >>= function
    | Some commit -> return commit
    | None ->
        I.update (store "Init") ["ck-version"] "0.1" >>= fun () ->
        I.head_exn (store "Get initial commit")

  let make store =
    get_current store >>= fun commit ->
    (* TODO: do all reads using this commit *)
    let disk_nodes = Hashtbl.create 100 in
    let children = Hashtbl.create 100 in
    Hashtbl.add disk_nodes Ck_id.root Ck_disk_node.root;
    I.list (store "Find db nodes") ["db"] >>=
    Lwt_list.iter_s (function
      | ["db"; uuid] as key ->
          let uuid = Ck_id.of_string uuid in
          assert (uuid <> Ck_id.root);
          I.read_exn (store "Load db node") key >|= fun s ->
          let node = Ck_disk_node.of_string s in
          Hashtbl.add disk_nodes uuid node;
          let parent = Ck_disk_node.parent node in
          let old_children =
            try Hashtbl.find children parent
            with Not_found -> [] in
          Hashtbl.replace children parent (uuid :: old_children);
      | _ -> assert false
    ) >>= fun () ->
    children |> Hashtbl.iter (fun parent children ->
      if not (Hashtbl.mem disk_nodes parent) then (
        error "Parent UUID '%a' of child nodes %s missing!" Ck_id.fmt parent (String.concat ", " (List.map Ck_id.to_string children))
      )
    );

    (* todo: reject cycles *)
    let rec make_node uuid =
      let disk_node = Hashtbl.find disk_nodes uuid in
      Node.make ~uuid ~disk_node ~child_nodes:(make_child_nodes uuid)
    and make_child_nodes uuid =
      begin try Hashtbl.find children uuid with Not_found -> [] end
      |> List.map make_node
      |> List.fold_left (fun set node ->
          M.add (Node.key node) node set
        ) M.empty in

    let root = Node.make_root ~child_nodes:(make_child_nodes Ck_id.root) in
    let index = Hashtbl.create 100 in
    root |> walk (fun node -> Hashtbl.add index (Node.uuid node) node);
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
    { store; commit; root; index; history}

  let get t uuid =
    try Some (Hashtbl.find t.index uuid)
    with Not_found -> None

  let get_exn t uuid =
    try Hashtbl.find t.index uuid
    with Not_found -> error "UUID '%a' not found in database!" Ck_id.fmt uuid

  (* Note: in theory, the result might not match the input type, if the
   * merge changes it for some reason. In practice, this shouldn't happen. *)
  let create t (node:_ Ck_disk_node.t) =
    let uuid = Ck_id.mint () in
    assert (not (Hashtbl.mem t.index uuid));
    let parent = Ck_disk_node.parent node in
    if not (Hashtbl.mem t.index parent) then
      error "Parent '%a' does not exist!" Ck_id.fmt parent;
    let s = Ck_disk_node.to_string node in
    let msg = Printf.sprintf "Create '%s'" (Ck_disk_node.name node) in
    I.update (t.store msg) ["db"; Ck_id.to_string uuid] s >>= fun () ->
    make t.store >|= fun t_new ->
    (Hashtbl.find t_new.index uuid, t_new)

  let update t ~msg node =
    let node = (node :> Node.generic) in
    assert (Hashtbl.mem t.index (Node.uuid node));
    if not (Hashtbl.mem t.index (Node.parent node)) then
      error "Parent '%a' does not exist!" Ck_id.fmt (Node.parent node);
      let s = Ck_disk_node.to_string node.Node.disk_node in
    I.update (t.store msg) ["db"; Ck_id.to_string node.Node.uuid] s >>= fun () ->
    make t.store

  let delete t uuid =
    assert (uuid <> Ck_id.root);
    let node = get_exn t uuid in
    let msg = Printf.sprintf "Delete '%s'" (Node.name node) in
    I.remove (t.store msg) ["db"; Ck_id.to_string uuid] >>= fun () ->
    make t.store
end

module Make(Clock : Ck_clock.S)(I : Irmin.BASIC with type key = string list and type value = string) = struct
  module R = Raw(I)

  type t = {
    current : R.t React.S.t;
    set_current : R.t -> unit;
  }

  type 'a full_node = 'a Node.t

  module View = struct
    type t = {
      uuid : Ck_id.t;
      init_node_type : [ area | project | action ];
      node_type : [ area | project | action | `Deleted ] React.S.t;
      ctime : float;
      name : string React.S.t;
      description : string React.S.t;
      child_views : t ReactiveData.RList.t;
      state : int Slow_set.state React.S.t;
    }

    let eq a b =
      a.uuid = b.uuid &&
      a.init_node_type = b.init_node_type &&
      a.ctime = b.ctime &&
      a.state == b.state
      (* We ignore the signals, since any view with the same
       * uuid with have the same signals values. *)
  end

  module Slow = Slow_set.Make(Clock)(Node.SortKey)(M)
  module NodeList = Delta_RList.Make(Node.SortKey)(View)(M)

  let assume_changed _ _ = false

  let make store =
    R.make store >|= fun r ->
    let current, set_current = React.S.create ~eq:R.eq r in
    { current; set_current }

  let root t = t.current |> React.S.map ~eq:assume_changed (fun r -> r.R.root)
  let is_root = (=) Ck_id.root

  let all_areas_and_projects t =
    let results = ref [] in
    let rec scan prefix x =
      let full_path = prefix ^ "/" ^ Node.name x in
      results := (full_path, x) :: !results;
      Node.child_nodes x |> M.iter (fun _k child ->
        match child with
        | {Node.disk_node = {Ck_disk_node.details = `Area | `Project _; _}; _} as x -> scan full_path x
        | _ -> ()
      ) in
    scan "" (root t |> React.S.value);
    List.rev !results

  let actions parent =
    let results = ref [] in
    Node.child_nodes parent |> M.iter (fun _k child ->
      match child with
      | {Node.disk_node = {Ck_disk_node.details = `Action _; _}; _} as x -> results := x :: !results
      | _ -> ()
    );
    List.rev !results

  let uuid = Node.uuid

  let add details t ~parent ~name ~description =
    let disk_node =
      Ck_disk_node.make ~name ~description ~parent ~ctime:(Unix.gettimeofday ()) ~details in
    let r = React.S.value t.current in
    R.create r disk_node >|= fun (node, r_new) ->
    t.set_current r_new;
    node.Node.uuid

  let add_action = add (`Action {Ck_sigs.astate = `Next})
  let add_project = add (`Project {Ck_sigs.pstate = `Active})
  let add_area = add `Area

  let delete t uuid =
    let r = React.S.value t.current in
    R.delete r uuid >|= t.set_current

  let set_name t uuid name =
    let r = React.S.value t.current in
    let node = R.get_exn r uuid in
    let new_node = Node.with_name node name in
    let msg = Printf.sprintf "Rename '%s' to '%s'" (Node.name node) (Node.name new_node) in
    R.update r ~msg new_node >|= t.set_current

  let set_details t uuid new_details =
    let r = React.S.value t.current in
    let node = R.get_exn r uuid in
    let new_node = Node.with_details node new_details in
    let msg = Printf.sprintf "Change state of '%s'" (Node.name node) in
    R.update r ~msg new_node >|= t.set_current

  let node_type {Node.disk_node = {Ck_disk_node.details; _}; _} = details
  let opt_node_type = function
    | None -> `Deleted
    | Some x -> (node_type x :> [action | project | area | `Deleted])
  let opt_node_name = function
    | None -> "(deleted)"
    | Some x -> Node.name x
  let opt_node_description = function
    | None -> "(deleted)"
    | Some x -> Node.description x
  let opt_child_nodes = function
    | None -> M.empty
    | Some x -> Node.child_nodes x

  type child_filter = {
(*     pred : Node.generic -> bool;        (* Whether to include a child *) *)
    render : (Node.generic, int) Slow_set.item -> View.t;    (* How to render it *)
  }

  let opt_node_eq a b =
    match a, b with
    | None, None -> true
    | Some a, Some b -> Node.eq a b
    | _ -> false

  let render_node ?child_filter t (node, state) =
    let live_node = t.current |> React.S.map ~eq:opt_node_eq (fun r -> R.get r node.Node.uuid) in
    let child_views =
      match child_filter with
      | None -> ReactiveData.RList.empty
      | Some filter -> live_node
          |> React.S.map ~eq:(M.equal Node.eq) opt_child_nodes
          |> Slow.make ~eq:Node.eq ~init:node.Node.child_nodes ~delay:1.0
          |> React.S.map ~eq:(M.equal View.eq) (M.map filter.render)
          |> NodeList.make in
    { View.
      uuid = Node.uuid node;
      ctime = Node.ctime node;
      init_node_type = Node.details node;
      node_type = live_node |> React.S.map opt_node_type;
      name = live_node |> React.S.map opt_node_name;
      description = live_node |> React.S.map opt_node_description;
      child_views;
      state;
    }

  let render_slow_node ?child_filter t item =
    let node = Slow_set.data item in
    let state = Slow_set.state item in
    render_node ?child_filter t (node, state)

  let process_tree t =
    let root_node = R.get_exn (React.S.value t.current) Ck_id.root in
    let rec child_filter = {
      render = (fun n -> render_slow_node ~child_filter t n);
    } in
    render_node t ~child_filter (root_node, React.S.const `Current)

  let collect_next_actions r =
    let results = ref M.empty in
    let rec scan = function
      | {Node.disk_node = {Ck_disk_node.details = `Area | `Project _; _}; _} as x ->
          results := actions x |> List.fold_left (fun set action ->
            match action with
            | {Node.disk_node = {Ck_disk_node.details = `Action {Ck_sigs.astate = `Next}; _}; _} ->
                M.add (Node.key action) (action :> Node.generic) set
            | _ -> set
          ) !results;
          Node.child_nodes x |> M.iter (fun _k v -> scan v)
      | {Node.disk_node = {Ck_disk_node.details = `Action _; _}; _} -> ()
    in
    scan r.R.root;
    !results

  let work_tree t =
    t.current
    |> React.S.map ~eq:(M.equal Node.eq) collect_next_actions
    |> Slow.make ~eq:Node.eq ~delay:1.0
    |> React.S.map ~eq:(M.equal View.eq) (M.map (render_slow_node t))
    |> NodeList.make

  let details t uuid =
    let initial_node = R.get_exn (React.S.value t.current) uuid in
    let child_filter = {
      render = render_slow_node t;
    } in
    render_node t ~child_filter (initial_node, React.S.const `Current)

  let history t =
    t.current >|~= fun r -> r.R.history
end
