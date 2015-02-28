(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Lwt

open Ck_utils
open Ck_sigs

module Node = Ck_node
module M = Ck_node.M

module Make(I : Irmin.BASIC with type key = string list and type value = string) = struct
  module Top = Graph.Topological.Make(I.History)

  type commit = float * string

  type t = {
    store : string -> I.t;
    commit : I.head;
    root : 'a. ([> area] as 'a) Node.t;
    index : (Ck_id.t, Node.generic) Hashtbl.t;
    history : commit list;
  }

  let equal a b =
    a.commit = b.commit

  let rec walk fn node =
    fn node;
    Node.child_nodes node |> M.iter (fun _k v -> walk fn v)

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
    (uuid, t_new)

  let update t ~msg node =
    let node = (node :> Node.generic) in
    assert (Hashtbl.mem t.index (Node.uuid node));
    if not (Hashtbl.mem t.index (Node.parent node)) then
      error "Parent '%a' does not exist!" Ck_id.fmt (Node.parent node);
      let s = Ck_disk_node.to_string (Node.disk_node node) in
    I.update (t.store msg) ["db"; Ck_id.to_string (Node.uuid node)] s >>= fun () ->
    make t.store

  let delete t node =
    let uuid = Node.uuid node in
    assert (uuid <> Ck_id.root);
    let node = get_exn t uuid in
    let msg = Printf.sprintf "Delete '%s'" (Node.name node) in
    I.remove (t.store msg) ["db"; Ck_id.to_string uuid] >>= fun () ->
    make t.store

  let add t ?uuid details ~parent ~name ~description =
    let disk_node =
      Ck_disk_node.make ~name ~description ~parent ~ctime:(Unix.gettimeofday ()) ~details in
    create ?uuid t disk_node

  let set_name t node name =
    let new_node = Node.with_name node name in
    let msg = Printf.sprintf "Rename '%s' to '%s'" (Node.name node) (Node.name new_node) in
    update t ~msg new_node

  let set_details t node new_details =
    let new_node = Node.with_details node new_details in
    let msg = Printf.sprintf "Change state of '%s'" (Node.name node) in
    update t ~msg new_node

  let set_action_state t node astate =
    match Node.disk_node node with
    | { Ck_disk_node.details = `Action old; _ } -> set_details t node (`Action {old with astate})

  let set_project_state t node pstate =
    match Node.disk_node node with
    | { Ck_disk_node.details = `Project old; _ } -> set_details t node (`Project {old with pstate})

  let set_starred t node s =
    let new_node =
      match Node.disk_node node with
      | {Ck_disk_node.details = `Action d; _} -> Node.with_details node (`Action {d with astarred = s})
      | {Ck_disk_node.details = `Project d; _} -> Node.with_details node (`Project {d with pstarred = s}) in
    let action = if s then "Add" else "Remove" in
    let msg = Printf.sprintf "%s star for '%s'" action (Node.name node) in
    update t ~msg new_node

  let root t = t.root
  let history t = t.history
end
