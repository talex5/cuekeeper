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
        mutable roots : generic_node M.t;
        index : (Ck_id.t, generic_node) Hashtbl.t;
        history : Git_storage_s.log_entry list;
      }
      and node_details = {
        rev : rev;
        uuid : Ck_id.t;
        child_nodes : generic_node M.t;
      }
      and action_node = Ck_disk_node.Types.action_node * node_details
      and project_node = Ck_disk_node.Types.project_node * node_details
      and area_node = Ck_disk_node.Types.area_node * node_details
      and generic_node =
        [ `Action of action_node
        | `Project of project_node
        | `Area of area_node ]

      type action = [`Action of action_node]
      type project = [`Project of project_node]
      type area = [`Area of area_node]
    end

    open Types

    type generic = generic_node

    let disk_node = function
      | `Action (d, _) -> `Action d
      | `Project (d, _) -> `Project d
      | `Area (d, _) -> `Area d

    let details = function
      | `Action (_, d) -> d
      | `Project (_, d) -> d
      | `Area (_, d) -> d

    let rev t = (details t).rev
    let parent t = Ck_disk_node.parent (disk_node t)
    let name t = Ck_disk_node.name (disk_node t)
    let description t = Ck_disk_node.description (disk_node t)
    let ctime t = Ck_disk_node.ctime (disk_node t)
    let action_state (d, _) = Ck_disk_node.action_state d
    let project_state (d, _) = Ck_disk_node.project_state d
    let starred = function
      | `Action (d, _) -> Ck_disk_node.starred (`Action d)
      | `Project (d, _) -> Ck_disk_node.starred (`Project d)
    let is_done = function
      | `Action (d, _) -> Ck_disk_node.is_done (`Action d)
      | `Project (d, _) -> Ck_disk_node.is_done (`Project d)

    let uuid t = (details t).uuid
    let child_nodes t = (details t).child_nodes

    let key node = (String.lowercase (name node), uuid node)

    let equal_excl_children a b =
      uuid a = uuid b &&
      disk_node a = disk_node b

    let rec equal a b =
      equal_excl_children a b &&
      M.equal equal (child_nodes a) (child_nodes b)
  end

  open Node.Types

  type rev = Node.Types.rev
  type t = rev

  let equal a b =
    a.commit = b.commit

  let rec walk fn =
    M.iter (fun _k v ->
      fn v;
      walk fn (Node.child_nodes v)
    )

  let make commit =
    Git.Commit.checkout commit >>= fun tree ->
    let disk_nodes = Hashtbl.create 100 in
    let children = Hashtbl.create 100 in
    Git.Staging.list tree ["db"] >>=
    Lwt_list.iter_s (function
      | ["db"; uuid] as key ->
          let uuid = Ck_id.of_string uuid in
          assert (uuid <> Ck_id.root);
          Git.Staging.read_exn tree key >|= fun s ->
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
      if parent <> Ck_id.root && not (Hashtbl.mem disk_nodes parent) then (
        error "Parent UUID '%a' of child nodes %s missing!" Ck_id.fmt parent (String.concat ", " (List.map Ck_id.to_string children))
      )
    );
    let index = Hashtbl.create 100 in
    Git.Commit.history ~depth:10 commit >>= fun history ->
    let t = { commit; roots = M.empty; index; history} in
    (* todo: reject cycles *)
    let rec make_node uuid =
      let details = {
        rev = t;
        uuid;
        child_nodes = make_child_nodes uuid;
      } in
      match Hashtbl.find disk_nodes uuid with
      | `Action n -> `Action (n, details)
      | `Project n -> `Project (n, details)
      | `Area n -> `Area (n, details)
    and make_child_nodes uuid =
      begin try Hashtbl.find children uuid with Not_found -> [] end
      |> List.map make_node
      |> List.fold_left (fun set node ->
          M.add (Node.key node) node set
        ) M.empty in
    let roots = make_child_nodes Ck_id.root in
    roots |> walk (fun node -> Hashtbl.add index (Node.uuid node) node);
    t.roots <- roots;
    return t

  let get t uuid =
    try Some (Hashtbl.find t.index uuid)
    with Not_found -> None

  let parent t node = get t (Node.parent node)

  let roots t = t.roots
  let commit t = t.commit
  let history t = t.history

  let disk_node = Node.disk_node
  let action_node (d, _) = d
  let project_node (d, _) = d
  let area_node (d, _) = d
end
