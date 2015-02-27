(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Ck_sigs

module SortKey = struct
  type t = string * Ck_id.t
  let compare (a_name, a_id) (b_name, b_id) =
    match String.compare a_name b_name with
    | 0 -> compare a_id b_id
    | r -> r
  let id = snd
  let show = fst
end
module M = Map.Make(SortKey)

type 'a t = {
  uuid : Ck_id.t;
  disk_node : 'a Ck_disk_node.t;
  child_nodes : generic M.t;
}
and generic = [action | project | area] t

let make ~uuid ~disk_node ~child_nodes = {
  uuid;
  disk_node;
  child_nodes;
}

let make_root ~child_nodes = make
  ~uuid:Ck_id.root
  ~disk_node:Ck_disk_node.root
  ~child_nodes

let parent t = Ck_disk_node.parent t.disk_node
let name t = Ck_disk_node.name t.disk_node
let description t = Ck_disk_node.description t.disk_node
let ctime t = Ck_disk_node.ctime t.disk_node
let details t = Ck_disk_node.details t.disk_node

let uuid t = t.uuid
let child_nodes t = t.child_nodes

let key node = (name node, uuid node)

let rec equal a b =
  a.uuid = b.uuid &&
  a.disk_node = b.disk_node &&
  M.equal equal a.child_nodes b.child_nodes

let with_name node name = {node with
  disk_node = Ck_disk_node.with_name node.disk_node name
}

let with_details node details = {node with
  disk_node = Ck_disk_node.with_details node.disk_node details
}
