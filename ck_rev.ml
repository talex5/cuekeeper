(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Lwt

open Ck_utils

module Make(Git : Git_storage_s.S) = struct
  type commit = Git.Commit.t

  module Node = struct
    module Types = struct
      type rev = {
        commit : Git.Commit.t;
        mutable roots : apa M.t;
        mutable children : apa M.t Ck_id.M.t;
        contexts : context_node Ck_id.M.t ref;
        contacts : contact_node Ck_id.M.t ref;
        actions_of_contact : (Ck_id.t, action_node) Hashtbl.t;
        actions_of_context : (Ck_id.t, action_node) Hashtbl.t;
        index : (Ck_id.t, apa) Hashtbl.t;
        mutable alert : bool;
        mutable schedule : action_node list;
        mutable expires : float option;
        valid_from : float;
      }
      and 'a node_details = {
        rev : rev;
        uuid : Ck_id.t;
        disk_node : 'a;
      }
      and action_node = Ck_disk_node.Types.action_node node_details
      and project_node = Ck_disk_node.Types.project_node node_details
      and area_node = Ck_disk_node.Types.area_node node_details
      and context_node = Ck_disk_node.Types.context_node node_details
      and contact_node = Ck_disk_node.Types.contact_node node_details
      and apa =
        [ `Action of action_node
        | `Project of project_node
        | `Area of area_node ]

      type action = [`Action of action_node]
      type project = [`Project of project_node]
      type area = [`Area of area_node]
      type contact = [`Contact of contact_node]
      type context = [`Context of context_node]
    end

    open Types

    type generic =
      [ apa
      | `Contact of contact_node
      | `Context of context_node ]

    let apa_disk_node = function
      | `Action n -> `Action n.disk_node
      | `Project n -> `Project n.disk_node
      | `Area n -> `Area n.disk_node

    let disk_node = function
      | `Action n -> `Action n.disk_node
      | `Project n -> `Project n.disk_node
      | `Area n -> `Area n.disk_node
      | `Contact n -> `Contact n.disk_node
      | `Context n -> `Context n.disk_node

    let details = function
      | `Action n -> {n with disk_node = ()}
      | `Project n -> {n with disk_node = ()}
      | `Area n -> {n with disk_node = ()}
      | `Contact n -> {n with disk_node = ()}
      | `Context n -> {n with disk_node = ()}

    let rev n = (details n).rev
    let uuid n = (details n).uuid

    let parent t = Ck_disk_node.parent (apa_disk_node t)
    let name t = Ck_disk_node.name (disk_node t)
    let description t = Ck_disk_node.description (disk_node t)
    let ctime t = Ck_disk_node.ctime (disk_node t)
    let action_state n = Ck_disk_node.action_state n.disk_node
    let context n = Ck_disk_node.context n.disk_node
    let project_state n = Ck_disk_node.project_state n.disk_node
    let starred = function
      | `Action n -> Ck_disk_node.starred (`Action n.disk_node)
      | `Project n -> Ck_disk_node.starred (`Project n.disk_node)
    let is_done = function
      | `Action n -> Ck_disk_node.is_done (`Action n.disk_node)
      | `Project n -> Ck_disk_node.is_done (`Project n.disk_node)

    let key node = (String.lowercase (name node), uuid node)

    let is_due action =
      match action_state action with
      | `Waiting_until time -> time <= action.rev.valid_from
      | _ -> false

    let node_due = function
      | `Action action -> is_due action
      | _ -> false

    let equal a b =
      uuid a = uuid b &&
      disk_node a = disk_node b &&
      node_due a = node_due b   (* Force the GUI to update when an item becomes due *)
  end

  open Node.Types

  type rev = Node.Types.rev
  type t = rev

  let equal a b =
    a.commit = b.commit

  let child_nodes node =
    let t = Node.rev node in
    let parent = Node.uuid node in
    try Ck_id.M.find parent t.children with Not_found -> M.empty

  let process_item ~now t = function
    | `Action a as node ->
        begin match Node.context a with
        | None -> ()
        | Some id ->
            if not (Ck_id.M.mem id !(t.contexts)) then
              error "Context '%a' of '%s' not found!" Ck_id.fmt id (Node.name node);
            Hashtbl.add t.actions_of_context id a end;
        begin match Node.action_state a with
        | `Waiting_for_contact c ->
            if not (Ck_id.M.mem c !(t.contacts)) then
              error "Contact '%a' of '%s' not found!" Ck_id.fmt c (Node.name node);
            Hashtbl.add t.actions_of_contact c a
        | `Waiting_until time ->
            t.schedule <- a :: t.schedule;
            if time <= now then t.alert <- true
            else (
              match t.expires with
              | Some old_time when old_time <= time -> ()
              | _ -> t.expires <- Some time
            )
        | _ -> () end
    | _ -> ()

  let make ~time commit =
    Git.Commit.checkout commit >>= fun tree ->
    let contacts = ref Ck_id.M.empty in
    let contexts = ref Ck_id.M.empty in
    let children = Hashtbl.create 100 in
    let index = Hashtbl.create 100 in
    let actions_of_contact = Hashtbl.create 10 in
    let actions_of_context = Hashtbl.create 10 in
    let t = {
      commit; roots = M.empty; contacts; index; actions_of_contact; children = Ck_id.M.empty;
      contexts; actions_of_context;
      schedule = []; alert = false; valid_from = time; expires = None
    } in
    (* Load areas, projects and actions *)
    Git.Staging.list tree ["db"] >>=
    Lwt_list.iter_s (function
      | ["db"; uuid] as key ->
          let uuid = Ck_id.of_string uuid in
          assert (uuid <> Ck_id.root);
          Git.Staging.read_exn tree key >|= fun s ->
          let disk_node = Ck_disk_node.of_string s in
          let node =
            match disk_node with
            | `Action disk_node -> `Action {rev = t; uuid; disk_node}
            | `Project disk_node -> `Project {rev = t; uuid; disk_node}
            | `Area disk_node -> `Area {rev = t; uuid; disk_node} in
          Hashtbl.add index uuid node;
          let parent = Ck_disk_node.parent disk_node in
          let old_children =
            try Hashtbl.find children parent
            with Not_found -> [] in
          Hashtbl.replace children parent (uuid :: old_children);
      | _ -> assert false
    ) >>= fun () ->
    (* Load contacts *)
    Git.Staging.list tree ["contact"] >>=
    Lwt_list.iter_s (function
      | ["contact"; uuid] as key ->
          let uuid = Ck_id.of_string uuid in
          Git.Staging.read_exn tree key >|= fun s ->
          let disk_node = Ck_disk_node.contact_of_string s in
          let contact = {rev = t; uuid; disk_node} in
          contacts := !contacts |> Ck_id.M.add uuid contact;
      | _ -> assert false
    ) >>= fun () ->
    (* Load contexts *)
    Git.Staging.list tree ["context"] >>=
    Lwt_list.iter_s (function
      | ["context"; uuid] as key ->
          let uuid = Ck_id.of_string uuid in
          Git.Staging.read_exn tree key >|= fun s ->
          let disk_node = Ck_disk_node.context_of_string s in
          let context = {rev = t; uuid; disk_node} in
          contexts := !contexts |> Ck_id.M.add uuid context;
      | _ -> assert false
    ) >>= fun () ->
    (* todo: reject cycles *)
    children |> Hashtbl.iter (fun parent children ->
      if parent <> Ck_id.root && not (Hashtbl.mem index parent) then (
        error "Parent UUID '%a' of child nodes %s missing!" Ck_id.fmt parent (String.concat ", " (List.map Ck_id.to_string children))
      )
    );
    t.children <-
      Hashtbl.fold (fun uuid child_uuids acc ->
        let child_map = child_uuids |> List.fold_left (fun acc child_uuid ->
          let child = Hashtbl.find index child_uuid in
          process_item ~now:time t child;
          acc |> M.add (Node.key child) child
        ) M.empty in
        acc |> Ck_id.M.add uuid child_map
      ) children Ck_id.M.empty;
    return t

  let get t uuid =
    try Some (Hashtbl.find t.index uuid)
    with Not_found -> None

  let get_contact t uuid =
    try Some (Ck_id.M.find uuid !(t.contacts))
    with Not_found -> None

  let get_context t uuid =
    try Some (Ck_id.M.find uuid !(t.contexts))
    with Not_found -> None

  let parent t node = get t (Node.parent node)

  let context action =
    match Node.context action with
    | None -> None
    | Some id -> get_context action.rev id

  let roots t =
    try Ck_id.M.find Ck_id.root t.children
    with Not_found -> M.empty

  let commit t = t.commit
  let contacts t = !(t.contacts)
  let contexts t = !(t.contexts)

  let actions_of_contact c =
    Hashtbl.find_all c.rev.actions_of_contact c.uuid

  let actions_of_context c =
    Hashtbl.find_all c.rev.actions_of_context c.uuid

  let disk_node = Node.disk_node
  let action_node n = n.disk_node
  let project_node n = n.disk_node
  let area_node n = n.disk_node

  let due action =
    match Node.action_state action with
    | `Waiting_until time -> time
    | _ -> assert false

  let by_due_time a b =
    compare (due a) (due b)

  let schedule t = List.sort by_due_time t.schedule
  let alert t = t.alert
  let expires t = t.expires
end
