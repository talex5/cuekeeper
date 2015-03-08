(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Lwt

open Ck_sigs
open Ck_utils

module Make(Git : Git_storage_s.S) = struct
  type commit = Git.Commit.t

  type t = {
    commit : Git.Commit.t;
    mutable roots : generic_node M.t;
    index : (Ck_id.t, generic_node) Hashtbl.t;
    history : Git_storage_s.log_entry list;
  }
  and 'a node = {
    rev : t;
    uuid : Ck_id.t;
    disk_node : 'a Ck_disk_node.t;
    child_nodes : generic_node M.t;
  }
  and generic_node = [action | project | area] node

  module Node = struct
    type 'a t = 'a node
    type generic = generic_node

    let rev t = t.rev
    let parent t = Ck_disk_node.parent t.disk_node
    let name t = Ck_disk_node.name t.disk_node
    let description t = Ck_disk_node.description t.disk_node
    let ctime t = Ck_disk_node.ctime t.disk_node
    let details t = Ck_disk_node.details t.disk_node
    let action_state t = Ck_disk_node.action_state t.disk_node
    let project_state t = Ck_disk_node.project_state t.disk_node
    let starred t = Ck_disk_node.starred t.disk_node

    let uuid t = t.uuid
    let child_nodes t = t.child_nodes

    let key node = (String.lowercase (name node), uuid node)

    let equal_excl_children a b =
      a.uuid = b.uuid &&
      a.disk_node = b.disk_node

    let rec equal a b =
      equal_excl_children a b &&
      M.equal equal a.child_nodes b.child_nodes

    let ty = function
      | { disk_node = { Ck_disk_node.details = `Action _; _ }; _ } as x -> `Action x
      | { disk_node = { Ck_disk_node.details = `Project _; _ }; _ } as x -> `Project x
      | { disk_node = { Ck_disk_node.details = `Area; _ }; _ } as x -> `Area x
  end

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
    Hashtbl.add disk_nodes Ck_id.root Ck_disk_node.root;
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
      if not (Hashtbl.mem disk_nodes parent) then (
        error "Parent UUID '%a' of child nodes %s missing!" Ck_id.fmt parent (String.concat ", " (List.map Ck_id.to_string children))
      )
    );
    let index = Hashtbl.create 100 in
    Git.Commit.history ~depth:10 commit >>= fun history ->
    let t = { commit; roots = M.empty; index; history} in
    (* todo: reject cycles *)
    let rec make_node uuid =
      let disk_node = Hashtbl.find disk_nodes uuid in
      {
        rev = t;
        uuid;
        disk_node;
        child_nodes = make_child_nodes uuid;
      }
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

  let disk_node n = n.disk_node
end
