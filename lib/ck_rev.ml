(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Lwt

open Ck_utils

module type S = sig
  include Ck_sigs.REV
  open Node.Types

  val make : time:Ck_time.user_date -> commit -> t Lwt.t
  val disk_node : [< Node.generic] -> Ck_disk_node.Types.node
  val apa_node : [< area | project | action] -> Ck_disk_node.Types.apa_node

  val action_node : action -> Ck_disk_node.Types.action_node
  val project_node : project -> Ck_disk_node.Types.project_node
  val area_node : area -> Ck_disk_node.Types.area_node
  val context_node : context -> Ck_disk_node.Types.context_node
  val contact_node : contact -> Ck_disk_node.Types.contact_node
end

module Make(Git : Git_storage_s.S) : S with type commit = Git.Commit.t = struct
  type commit = Git.Commit.t

  module Node = struct
    module Types = struct
      open Ck_disk_node.Types

      type rev = {
        commit : Git.Commit.t;
        mutable children : apa_node M.t Ck_id.M.t;
        contexts : context_node Ck_id.M.t ref;
        contacts : contact_node Ck_id.M.t ref;
        nodes_of_contact : (Ck_id.t, apa_node) Hashtbl.t;
        actions_of_context : (Ck_id.t, action_node) Hashtbl.t;
        apa_nodes : apa_node Ck_id.M.t ref;
        mutable alert : bool;
        mutable schedule : action_node list;
        mutable expires : Ck_time.user_date option;
        valid_from : Ck_time.user_date;
        mutable problems : (Ck_id.t, string) Hashtbl.t
      }

      type 'a node_details = {
        rev : rev;
        disk_node : 'a;
      }

      type apa =
        [ `Action of action_node node_details
        | `Project of project_node node_details
        | `Area of area_node node_details]
      and action = [`Action of action_node node_details]
      and project = [`Project of project_node node_details]
      and area = [`Area of area_node node_details]
      and contact = [`Contact of contact_node node_details]
      and context = [`Context of context_node node_details]

      type action_node = Ck_disk_node.Types.action_node node_details
      type project_node = Ck_disk_node.Types.project_node node_details
      type area_node = Ck_disk_node.Types.area_node node_details
      type contact_node = Ck_disk_node.Types.contact_node node_details
      type context_node = Ck_disk_node.Types.context_node node_details
    end

    open Types

    type 'a node_details = 'a Types.node_details

    let apa_ty ({rev; disk_node} : #Ck_disk_node.Types.apa_node node_details) =
      match disk_node#ty with
      | `Area disk_node -> `Area {rev; disk_node}
      | `Project disk_node -> `Project {rev; disk_node}
      | `Action disk_node -> `Action {rev; disk_node}

    type generic =
      [ apa
      | `Contact of Ck_disk_node.Types.contact_node node_details
      | `Context of Ck_disk_node.Types.context_node node_details ]

    let apa_disk_node : [< apa] -> Ck_disk_node.Types.apa_node = function
      | `Action n -> (n.disk_node :> Ck_disk_node.Types.apa_node)
      | `Project n -> (n.disk_node :> Ck_disk_node.Types.apa_node)
      | `Area n -> (n.disk_node :> Ck_disk_node.Types.apa_node)

    let disk_node : [< generic] -> Ck_disk_node.Types.node = function
      | `Action n -> (n.disk_node :> Ck_disk_node.Types.node)
      | `Project n -> (n.disk_node :> Ck_disk_node.Types.node)
      | `Area n -> (n.disk_node :> Ck_disk_node.Types.node)
      | `Contact n -> (n.disk_node :> Ck_disk_node.Types.node)
      | `Context n -> (n.disk_node :> Ck_disk_node.Types.node)

    let rev = function
      | `Action n -> n.rev
      | `Project n -> n.rev
      | `Area n -> n.rev
      | `Contact n -> n.rev
      | `Context n -> n.rev

    let uuid n = (disk_node n)#uuid
    let contact t = (apa_disk_node t)#contact
    let parent t = (apa_disk_node t)#parent
    let name t = (disk_node t)#name
    let description t = (disk_node t)#description
    let ctime t = (disk_node t)#ctime
    let conflicts t = (disk_node t)#conflicts
    let action_state (`Action n) = n.disk_node#state
    let action_repeat (`Action n) = n.disk_node#repeat
    let context (`Action n) = n.disk_node#context
    let project_state (`Project n) = n.disk_node#state
    let starred = function
      | `Action n -> n.disk_node#starred
      | `Project n -> n.disk_node#starred
    let is_done = function
      | `Action n -> n.disk_node#state = `Done
      | `Project n -> n.disk_node#state = `Done

    let key_priv node = (String.lowercase node#name, node#uuid)
    let key node = key_priv (disk_node node)

    let is_due action =
      match action_state action with
      | `Waiting_until time -> Ck_time.compare time (rev action).valid_from <= 0
      | _ -> false

    let node_due = function
      | `Action _ as action -> is_due action
      | _ -> false

    let equal a b =
      let a = (a :> generic) in
      let b = (b :> generic) in
      uuid a = uuid b &&
      Ck_disk_node.equal (disk_node a) (disk_node b) &&
      node_due a = node_due b   (* Force the GUI to update when an item becomes due *)
  end

  open Node.Types

  type rev = Node.Types.rev
  type t = rev

  let equal a b =
    Git.Commit.equal a.commit b.commit &&
    a.expires = b.expires

  let export_apa t disk_node =
    match disk_node#apa_ty with
    | `Area n -> `Area {rev = t; disk_node = n}
    | `Project n -> `Project {rev = t; disk_node = n}
    | `Action n -> `Action {rev = t; disk_node = n}

  let export_generic t disk_node =
    match disk_node#ty with
    | `Area n -> `Area {rev = t; disk_node = n}
    | `Project n -> `Project {rev = t; disk_node = n}
    | `Action n -> `Action {rev = t; disk_node = n}
    | `Contact n -> `Contact {rev = t; disk_node = n}
    | `Context n -> `Context {rev = t; disk_node = n}

  let child_nodes_priv t disk_node =
    let parent = disk_node#uuid in
    try Ck_id.M.find parent t.children with Not_found -> M.empty

  let child_nodes node =
    let t = Node.rev node in
    child_nodes_priv t (Node.disk_node node)
    |> M.map (export_apa t)

  let process_item ~now t (node:#Ck_disk_node.Types.node) =
    begin match node#contact with
    | None -> ()
    | Some c ->
        if not (Ck_id.M.mem c !(t.contacts)) then
          bug "Contact '%a' of '%s' not found!" Ck_id.fmt c node#name;
        Hashtbl.add t.nodes_of_contact c node end;
    match node#ty with
    | `Action node ->
        begin match node#context with
        | None -> ()
        | Some id ->
            if not (Ck_id.M.mem id !(t.contexts)) then
              bug "Context '%a' of '%s' not found!" Ck_id.fmt id node#name
            Hashtbl.add t.actions_of_context id node end;
        begin match node#state with
        | `Waiting_for_contact ->
            if node#contact = None then
              bug "Waiting_for_contact but no contact set on '%s'" node#name
        | `Waiting_until time ->
            t.schedule <- node :: t.schedule;
            if time <= now then t.alert <- true
            else (
              match t.expires with
              | Some old_time when Ck_time.compare old_time time <= 0 -> ()
              | _ -> t.expires <- Some time
            )
        | _ -> () end
    | _ -> ()

  type active = In_progress | Idle

  let is_area n =
    match n#apa_ty with
    | `Area _ -> true
    | _ -> false

  let roots_priv t =
    try Ck_id.M.find Ck_id.root t.children
    with Not_found -> M.empty

  let roots t =
    roots_priv t |> M.map (export_apa t)

  let ensure_no_cycles t =
    let rec aux unseen nodes =
      M.fold (fun _key node unseen ->
        let unseen = unseen |> Ck_id.M.remove node#uuid in
        aux unseen (child_nodes_priv t node)
      ) nodes unseen in
    let unreachable = aux !(t.apa_nodes) (roots_priv t) in
    if not (Ck_id.M.is_empty unreachable) then (
      let _id, example = Ck_id.M.min_binding unreachable in
      bug "Node '%s' is not reachable from the root! (cycle in parent relation)" example#name
    )

  let is_incomplete n =
    match n#apa_ty with
    | `Area _ -> true
    | `Project n -> n#state <> `Done
    | `Action n -> n#state <> `Done

  let check_for_problems t =
    let rec scan node =
      let add problem =
        Hashtbl.add t.problems node#uuid problem in
      let bug problem =
        Ck_utils.bug "Bad item '%s': %s" node#name problem in
      let reduce_progress _ child acc =
        match scan child with
        | Idle -> acc
        | In_progress -> In_progress in
      if node#conflicts <> [] then add "Unread merge conflicts report";
      let child_nodes = child_nodes_priv t node in
      match node#apa_ty with
      | `Project node ->
          if (M.exists (fun _k -> is_area) child_nodes) then bug "Project with area child!";
          if node#state = `Done && M.exists (fun _k -> is_incomplete) child_nodes then
            add "Completed project with incomplete child";
          let children_status = M.fold reduce_progress child_nodes Idle in
          if children_status = Idle && node#state = `Active then add "Active project with no next action";
          children_status
      | `Area _ ->
          M.fold reduce_progress child_nodes Idle
      | `Action node ->
          if not (M.is_empty child_nodes) then bug "Action with children!";
          begin match node#state with
          | `Next | `Waiting_for_contact | `Waiting_until _ | `Waiting -> In_progress
          | `Future -> Idle
          | `Done ->
              if node#repeat <> None then bug "Repeating action marked as done!";
              Idle end
      in
    roots_priv t |> M.iter (fun _k n -> ignore (scan n))

  let make_no_cache ~time commit =
    Git.Commit.checkout commit >>= fun tree ->
    let contacts = ref Ck_id.M.empty in
    let contexts = ref Ck_id.M.empty in
    let children = Hashtbl.create 100 in
    let apa_nodes = ref Ck_id.M.empty in
    let nodes_of_contact = Hashtbl.create 10 in
    let actions_of_context = Hashtbl.create 10 in
    let t = {
      commit; contacts; apa_nodes; nodes_of_contact; children = Ck_id.M.empty;
      contexts; actions_of_context;
      schedule = []; alert = false; valid_from = time; expires = None;
      problems = Hashtbl.create 0;
    } in
    (* Load areas, projects and actions *)
    Git.Staging.list tree ["db"] >>=
    Lwt_list.iter_s (function
      | ["db"; uuid] as key ->
          let uuid = Ck_id.of_string uuid in
          assert (uuid <> Ck_id.root);
          Git.Staging.read_exn tree key >|= fun s ->
          let disk_node = Ck_disk_node.apa_of_string ~uuid s in
          apa_nodes := !apa_nodes |> Ck_id.M.add uuid disk_node;
          let parent = disk_node#parent in
          let old_children =
            try Hashtbl.find children parent
            with Not_found -> [] in
          Hashtbl.replace children parent (uuid :: old_children);
      | _ -> assert false
    ) >>= fun () ->
    let apa_nodes = !apa_nodes in
    (* Load contacts *)
    Git.Staging.list tree ["contact"] >>=
    Lwt_list.iter_s (function
      | ["contact"; uuid] as key ->
          let uuid = Ck_id.of_string uuid in
          Git.Staging.read_exn tree key >|= fun s ->
          let contact = Ck_disk_node.contact_of_string ~uuid s in
          contacts := !contacts |> Ck_id.M.add uuid contact;
      | _ -> assert false
    ) >>= fun () ->
    (* Load contexts *)
    Git.Staging.list tree ["context"] >>=
    Lwt_list.iter_s (function
      | ["context"; uuid] as key ->
          let uuid = Ck_id.of_string uuid in
          Git.Staging.read_exn tree key >|= fun s ->
          let context = Ck_disk_node.context_of_string ~uuid s in
          contexts := !contexts |> Ck_id.M.add uuid context;
      | _ -> assert false
    ) >>= fun () ->
    (* todo: reject cycles *)
    children |> Hashtbl.iter (fun parent children ->
      if parent <> Ck_id.root && not (Ck_id.M.mem parent apa_nodes) then (
        bug "Parent UUID '%a' of child nodes %s missing!" Ck_id.fmt parent (String.concat ", " (List.map Ck_id.to_string children))
      )
    );
    t.children <-
      Hashtbl.fold (fun uuid child_uuids acc ->
        let child_map = child_uuids |> List.fold_left (fun acc child_uuid ->
          let child = Ck_id.M.find child_uuid apa_nodes in
          process_item ~now:time t child;
          acc |> M.add (Node.key_priv child) child
        ) M.empty in
        acc |> Ck_id.M.add uuid child_map
      ) children Ck_id.M.empty;
    ensure_no_cycles t;
    check_for_problems t;
    if Hashtbl.length t.problems > 0 then t.alert <- true;
    return t

  let last = ref None
  let make ~time commit =
    let reload () =
      make_no_cache ~time commit >|= fun r ->
      last := Some r;
      r in
    match !last with
    | Some last when Git.Commit.equal last.commit commit ->
        begin match last.expires with
        | Some etime when time >= etime -> reload ()
        | _ -> return last end
    | _ -> reload ()

  let nodes t = !(t.apa_nodes) |> Ck_id.M.map (export_apa t)

  let get t uuid =
    try
      let node = Ck_id.M.find uuid !(t.apa_nodes) in
      Some (export_apa t node)
    with Not_found -> None

  let get_contact t uuid =
    try Some (`Contact {rev = t; disk_node = Ck_id.M.find uuid !(t.contacts)})
    with Not_found -> None

  let get_context t uuid =
    try Some (`Context {rev = t; disk_node = Ck_id.M.find uuid !(t.contexts)})
    with Not_found -> None

  let parent t node = get t (Node.parent node)

  let context (`Action node as action) =
    match Node.context action with
    | None -> None
    | Some id -> get_context node.rev id

  let commit t = t.commit
  let contacts t = !(t.contacts) |> Ck_id.M.map (fun c -> `Contact {rev = t; disk_node = c})
  let contexts t = !(t.contexts) |> Ck_id.M.map (fun c -> `Context {rev = t; disk_node = c})

  let nodes_of_contact (`Contact c) =
    let t = c.rev in
    Hashtbl.find_all c.rev.nodes_of_contact c.disk_node#uuid
    |> List.map (export_apa t)

  let actions_of_context (`Context c) =
    let t = c.rev in
    Hashtbl.find_all c.rev.actions_of_context c.disk_node#uuid
    |> List.map (fun a -> `Action {rev = t; disk_node = a})

  let contact_for node =
    match Node.contact node with
    | None -> None
    | Some id -> get_contact (Node.rev node) id

  let disk_node = Node.disk_node
  let apa_node = Node.apa_disk_node
  let action_node (`Action n) = n.disk_node
  let project_node (`Project n) = n.disk_node
  let area_node (`Area n) = n.disk_node
  let contact_node (`Contact n) = n.disk_node
  let context_node (`Context n) = n.disk_node

  let due action =
    match action#state with
    | `Waiting_until time -> time
    | _ -> assert false

  let by_due_time a b =
    compare (due a) (due b)

  let schedule t = List.sort by_due_time t.schedule |> List.map (fun a -> `Action {rev = t; disk_node = a})
  let alert t = t.alert
  let expires t = t.expires

  let problems t =
    let problems = ref [] in
    t.problems |> Hashtbl.iter (fun uuid msg ->
      let node = Ck_id.M.find uuid !(t.apa_nodes) in
      problems := (export_generic t node, msg) :: !problems
    );
    !problems
end
