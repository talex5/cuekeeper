(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

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
  let create t ?uuid (node:_ Ck_disk_node.t) =
    let uuid =
      match uuid with
      | Some uuid -> uuid
      | None -> Ck_id.mint () in
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

  module TreeNode = struct
    module Id_map = Ck_id.M
    module Child_map = M
    module Sort_key = Node.SortKey

    module Item = struct
      type t = Node.generic   (* Ignore children, though *)

      let equal a b =
        Node.uuid a = Node.uuid b &&
        Ck_disk_node.equal a.Node.disk_node b.Node.disk_node

      let show = Node.name
      let id = Node.uuid
      let node n = n.Node.disk_node
    end

    type t = {
      item : Node.generic;
      children : t M.t;
    }

    let item t = t.item
    let children t = t.children

    let leaf_of_node n = {
      item = n;
      children = M.empty;
    }

    let id t = Item.id t.item

    let rec equal a b =
      Item.equal a.item b.item &&
      Child_map.equal equal a.children b.children

    type move_data = int
  end
  module WidgetTree = Reactive_tree.Make(Clock)(TreeNode)

  module Item = TreeNode.Item
  module Widget = WidgetTree.Widget

  type t = {
    current : R.t React.S.t;
    set_current : R.t -> unit;
    work_tree : WidgetTree.t;
    process_tree : WidgetTree.t;
    keep_me : unit React.S.t list;
  }

  type 'a full_node = 'a Node.t

  let assume_changed _ _ = false

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

  let uuid = Node.uuid

  let add details t ~parent ~name ~description =
    let disk_node =
      Ck_disk_node.make ~name ~description ~parent ~ctime:(Unix.gettimeofday ()) ~details in
    let r = React.S.value t.current in
    R.create r disk_node >|= fun (node, r_new) ->
    t.set_current r_new;
    node.Node.uuid

  let add_action = add (`Action {Ck_sigs.astate = `Next; astarred = false})
  let add_project = add (`Project {Ck_sigs.pstate = `Active; pstarred = false})
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

  let set_starred t uuid s =
    let r = React.S.value t.current in
    let node = R.get_exn r uuid in
    let new_node =
      match node with
      | {Node.disk_node = {Ck_disk_node.details = `Action d; _}; _} as n -> Node.with_details n (`Action {d with astarred = s})
      | {Node.disk_node = {Ck_disk_node.details = `Project d; _}; _} as n -> Node.with_details n (`Project {d with pstarred = s})
      | _ -> error "Not a project or action node: '%s'" (Node.name node) in
    let action = if s then "Add" else "Remove" in
    let msg = Printf.sprintf "%s star for '%s'" action (Node.name node) in
    R.update r ~msg new_node >|= t.set_current

  let make_full_tree r =
    let rec aux items =
      items |> M.map (fun item ->
        { TreeNode.item;
          children = aux (Node.child_nodes item) }
      ) in
    aux (r.R.root.Node.child_nodes)

  let make_process_tree = make_full_tree

  let is_next_action _k = function
    | {Node.disk_node = {Ck_disk_node.details = `Action {astate = `Next; _}; _}; _} -> true
    | _ -> false

  let collect_next_actions r =
    let results = ref TreeNode.Child_map.empty in
    let rec scan = function
      | {Node.disk_node = {Ck_disk_node.details = `Area | `Project _; _}; _} as parent ->
          let actions = Node.child_nodes parent |> M.filter is_next_action in
          if not (M.is_empty actions) then (
            let tree_node = { TreeNode.
              item = parent;
              children = actions |> M.map TreeNode.leaf_of_node
            } in
            results := !results |> M.add (Node.key parent) tree_node;
          );
          Node.child_nodes parent |> M.iter (fun _k v -> scan v)
      | {Node.disk_node = {Ck_disk_node.details = `Action _; _}; _} -> ()
    in
    scan r.R.root;
    !results

  let work_tree t = WidgetTree.widgets t.work_tree
  let process_tree t = WidgetTree.widgets t.process_tree

  type details = {
    details_item : Item.t option React.S.t;
    details_children : Widget.t ReactiveData.RList.t;
    details_stop : stop;
  }

  let details t uuid =
    let initial_node = R.get_exn (React.S.value t.current) uuid in
    let child_nodes node = Node.child_nodes node |> M.map TreeNode.leaf_of_node in
    let children = WidgetTree.make (child_nodes initial_node) in
    let node, set_node = React.S.create (Some initial_node) in
    let updates =
      t.current >|~= fun r ->
        let node = R.get r uuid in
        set_node node;
        match node with
        | None -> WidgetTree.update children M.empty
        | Some node -> WidgetTree.update children (child_nodes node) in
    {
      details_item = node;
      details_children = WidgetTree.widgets children;
      details_stop = (fun () -> ignore updates; ignore children);
    }

  let history t =
    t.current >|~= fun r -> r.R.history

  let initialise t =
    let add ~uuid ?parent ~name ~description details =
      let parent =
        match parent with
        | None -> Ck_id.root
        | Some p -> p in
      let disk_node =
        Ck_disk_node.make ~name ~description ~parent ~ctime:(Unix.gettimeofday ()) ~details in
      let r = React.S.value t.current in
      R.create r ~uuid:(Ck_id.of_string uuid) disk_node >|= fun (node, r_new) ->
      t.set_current r_new;
      node.Node.uuid in
    (* Add some default entries for first-time use.
     * Use fixed UUIDs for unit-testing and in case we want to merge stores later. *)
    add
      ~uuid:"ad8c5bb1-f6b7-4a57-b090-d6ef2e3326c1"
      ~name:"Personal"
      ~description:"Add personal sub-areas here (Family, Car, Home, Exercise, etc)."
      `Area
    >>= fun personal ->

    add
      ~uuid:"1a7c8ea2-18ac-41cb-8f79-3566e49445f4"
      ~parent:personal
      ~name:"Start using CueKeeper"
      ~description:""
      (`Project {pstate = `Active; pstarred = false})
    >>= fun switch_to_ck ->

    add
      ~uuid:"6002ea71-6f1c-4ba9-8728-720f4b4c9845"
      ~parent:switch_to_ck
      ~name:"Read wikipedia page on GTD"
      ~description:"http://en.wikipedia.org/wiki/Getting_Things_Done"
      (`Action {astate = `Next; astarred = false})
    >>= fun _ ->

    add
      ~uuid:"1c6a6964-e6c8-499a-8841-8cb437e2930f"
      ~name:"Work"
      ~description:"Add work-related sub-areas here."
      `Area
    >>= fun _ ->
    return ()

  let make store =
    R.make store >>= fun r ->
    let current, set_current = React.S.create ~eq:R.eq r in
    let process_tree = WidgetTree.make (make_process_tree r) in
    let update_process_tree =
      current
      |> React.S.map ~eq:(M.equal TreeNode.equal) make_process_tree
      |> React.S.map (WidgetTree.update process_tree) in
    let work_tree = WidgetTree.make (collect_next_actions r) in
    let update_work_tree =
      current
      |> React.S.map ~eq:(M.equal TreeNode.equal) collect_next_actions
      |> React.S.map (WidgetTree.update work_tree) in
    let keep_me = [update_work_tree; update_process_tree] in
    let t = { current; set_current; work_tree; process_tree; keep_me } in
    if M.is_empty (Node.child_nodes r.R.root) then (
      initialise t >>= fun () -> return t
    ) else return t
end
