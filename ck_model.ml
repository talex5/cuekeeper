(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Lwt

open Ck_utils
open Ck_sigs

module Node = Ck_node
module M = Ck_node.M

module Make(Clock : Ck_clock.S)(I : Irmin.BASIC with type key = string list and type value = string) = struct
  module R = Ck_rev.Make(I)

  module TreeNode = struct
    module Id_map = Ck_id.M
    module Child_map = M
    module Sort_key = Node.SortKey

    module Item = struct
      include Node    (* We reuse Node.t, but ignore its children *)

      let equal a b =
        Node.uuid a = Node.uuid b &&
        Ck_disk_node.equal (Node.disk_node a) (Node.disk_node b)
      let show = name
      let id = Node.uuid
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

  let root t = t.current |> React.S.map ~eq:assume_changed R.root
  let is_root = (=) Ck_id.root

(*
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
*)

  let add details t ~parent ~name ~description =
    let r = React.S.value t.current in
    R.add r details ~parent ~name ~description >|= fun (new_id, r_new) ->
    t.set_current r_new;
    new_id

  let add_action = add (`Action {Ck_sigs.astate = `Next; astarred = false})
  let add_project = add (`Project {Ck_sigs.pstate = `Active; pstarred = false})
  let add_area = add `Area

  let delete t node =
    let r = React.S.value t.current in
    R.delete r node >|= t.set_current

  let set_name t uuid name =
    let r = React.S.value t.current in
    let node = R.get_exn r uuid in
    R.set_name r node name >|= t.set_current

  let set_details t uuid new_details =
    let r = React.S.value t.current in
    let node = R.get_exn r uuid in
    R.set_details r node new_details >|= t.set_current

  let set_action_state t item state =
    let r = React.S.value t.current in
    R.set_action_state r item state >|= t.set_current

  let set_project_state t item state =
    let r = React.S.value t.current in
    R.set_project_state r item state >|= t.set_current

  let set_starred t item s =
    let r = React.S.value t.current in
    R.set_starred r item s >|= t.set_current

  let make_full_tree r =
    let rec aux items =
      items |> M.map (fun item ->
        { TreeNode.item;
          children = aux (Node.child_nodes item) }
      ) in
    aux (Node.child_nodes (R.root r))

  let make_process_tree = make_full_tree

  let is_next_action _k node =
    match Node.ty node with
    | `Action a -> Node.action_state a = `Next
    | _ -> false

  let collect_next_actions r =
    let results = ref TreeNode.Child_map.empty in
    let rec scan node =
      match Node.ty node with
      | `Area parent | `Project parent ->
          let actions = Node.child_nodes parent |> M.filter is_next_action in
          if not (M.is_empty actions) then (
            let tree_node = { TreeNode.
              item = parent;
              children = actions |> M.map TreeNode.leaf_of_node
            } in
            results := !results |> M.add (Node.key parent) tree_node;
          );
          Node.child_nodes parent |> M.iter (fun _k v -> scan v)
      | `Action _ -> ()
    in
    scan (R.root r);
    !results

  let work_tree t = WidgetTree.widgets t.work_tree
  let process_tree t = WidgetTree.widgets t.process_tree

  type details = {
    details_item : Item.generic option React.S.t;
    details_children : Widget.t ReactiveData.RList.t;
    details_stop : stop;
  }

  let details t uuid =
    let initial_node = R.get_exn (React.S.value t.current) uuid in
    let child_nodes node = Node.child_nodes node |> M.map TreeNode.leaf_of_node in
    let children = WidgetTree.make (child_nodes initial_node) in
    let node, set_node = React.S.create (Some initial_node) in
    let updates : unit React.S.t =
      t.current >|~= fun r ->
        let node = R.get r uuid in
        set_node node;
        match node with
        | None -> WidgetTree.update children M.empty ~on_remove:(R.get r)
        | Some node -> WidgetTree.update children (child_nodes node) ~on_remove:(R.get r) in
    {
      details_item = node;
      details_children = WidgetTree.widgets children;
      details_stop = (fun () -> ignore updates; ignore children);
    }

  let history t =
    t.current >|~= R.history

  let initialise t =
    let add ~uuid ?parent ~name ~description details =
      let parent =
        match parent with
        | None -> Ck_id.root
        | Some p -> p in
      let r = React.S.value t.current in
      R.add r ~uuid:(Ck_id.of_string uuid) details ~parent ~name ~description >|= fun (uuid, r_new) ->
      t.set_current r_new;
      uuid in
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

  let make_tree current fn =
    let rtree = WidgetTree.make (fn (React.S.value current)) in
    let keep_me =
      current
      |> React.S.map ~eq:(M.equal TreeNode.equal) fn
      |> React.S.map (fun tree ->
          let r = React.S.value current in
          WidgetTree.update rtree tree ~on_remove:(R.get r)
        ) in
    (rtree, keep_me)

  let make store =
    R.make store >>= fun r ->
    let current, set_current = React.S.create ~eq:R.equal r in
    let process_tree, update_process_tree = make_tree current make_process_tree in
    let work_tree, update_work_tree = make_tree current collect_next_actions in
    let keep_me = [update_work_tree; update_process_tree] in
    let t = { current; set_current; work_tree; process_tree; keep_me } in
    if M.is_empty (Node.child_nodes (R.root r)) then (
      initialise t >>= fun () -> return t
    ) else return t
end
