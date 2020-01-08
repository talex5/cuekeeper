(* Copyright (C) 2015, Thomas Leonard.
 * See the README file for details. *)

open Lwt.Infix

module IO = struct
  type in_channel = unit
  type out_channel = Buffer.t
  let really_input _ch _buf _pos _len = failwith "unused"
  let input = really_input
  let output = Buffer.add_subbytes
  let close_out _ = ()
end

let option_map f = function
  | None -> None
  | Some x -> Some (f x)

module T = Tar.Make(IO)

module Make (I : Irmin.S
             with type key = string list
              and type contents = string
              and type Commit.Hash.t = Irmin.Hash.SHA1.t
              and type branch = string
              and type step = string) = struct
  let bundle_t = Irmin.Type.pair I.Private.Slice.t I.Commit.Hash.t

  type repo = {
    r : I.Repo.t;
    info_maker : string -> Irmin.Info.t;
  }

  module Staging = struct
    type t = {
      repo : repo;
      parents : I.Commit.t list;
      mutable view : I.tree;
    }

    let create repo ~parents ~tree =
      {repo; parents; view = tree }

    let list t path = I.Tree.list t.view path >|= List.map fst
    let read t = I.Tree.find t.view
    let read_exn t = I.Tree.get t.view

    let update t k v =
      I.Tree.add t.view k v >|= fun v2 ->
      t.view <- v2

    let remove t k =
      I.Tree.remove t.view k >|= fun v2 ->
      t.view <- v2

    let mem t = I.Tree.mem t.view
  end

  module Commit = struct
    type t = {
      repo : repo;
      commit : I.Commit.t;
    }

    type id = I.Commit.Hash.t

    let v repo commit = { repo; commit }

    let id t = I.Commit.hash t.commit

    let equal a b =
      id a = id b

    let checkout t =
      I.Commit.tree t.commit >|= fun tree ->
      Staging.create t.repo ~parents:[t.commit] ~tree

    let commit ?parents staging ~msg =
      let repo = staging.Staging.repo in
      let parents =
        match parents with
        | Some parents -> List.map (fun t -> t.commit) parents
        | None -> staging.Staging.parents
      in
      let info =
        match msg with
        | [] -> failwith "Empty commit message!"
        | [summary] -> repo.info_maker summary
        | summary :: body -> repo.info_maker (summary ^ "\n" ^ String.concat "\n" body)
      in
      I.Commit.v repo.r ~info ~parents staging.Staging.view >|= fun commit ->
      { repo; commit }

    let history ?depth t =
      let open Git_storage_s in
      let module Top = Graph.Topological.Make_stable(struct
        type t = I.History.t
        let in_degree = I.History.in_degree
        let iter_succ = I.History.iter_succ
        let iter_vertex = I.History.iter_vertex
        module V = struct
          include I.History.V
          let compare a b =
            let ta = I.Commit.info a in
            let tb = I.Commit.info b in
            match Int64.compare (Irmin.Info.date ta) (Irmin.Info.date tb) with
            | 0 -> I.History.V.compare a b
            | r -> r
        end
      end) in

      I.of_commit t.commit >>= I.history ?depth >>= fun history ->
      (* Set rank field according to topological order and build final result map *)
      let map = ref Log_entry_map.empty in
      let rank = ref 0 in
      history |> Top.iter (fun commit ->
        let info = I.Commit.info commit in
        incr rank;
        let msg = Irmin.Info.message info in
        let date = Irmin.Info.date info |> Int64.to_float in
        let id = I.Commit.hash commit in
        let entry = { Log_entry.date; rank = !rank; msg; id } in
        map := !map |> Log_entry_map.add entry entry
      );
      Lwt.return !map

    let merge a b =
      I.of_commit a.commit >>= fun tmp ->
      let info () = a.repo.info_maker "Merge" in
      I.merge_with_commit tmp b.commit ~info >>= function
      | Ok () -> I.Head.get tmp >|= fun commit -> `Ok { a with commit }
      | Error (`Conflict _ as c) -> Lwt.return c

    let export_tar t =
      let buf = Buffer.create 10240 in
      let files = ref [] in
      let rec scan ~path tree =
        I.Tree.list tree [] >>= fun items ->
        items |> Lwt_list.iter_s @@ function
        | step, `Contents ->
          I.Tree.get tree [step] >>= fun data ->
          let header = T.Header.make
              ~file_mode:0o644
              (String.concat "/" (path @ [step])) (String.length data |> Int64.of_int) in
          let write b = Buffer.add_string b data in
          files := (header, write) :: !files;
          Lwt.return_unit
        | step, `Node ->
          I.Tree.get_tree tree [step] >>= scan ~path:(path @ [step])
      in
      (* Cannot use [I.Tree.to_concrete] here due to https://github.com/mirage/irmin/pull/525 *)
      I.Commit.tree t.commit >>= fun root ->
      scan ~path:[] root >|= fun () ->
      T.Archive.create_gen (Stream.of_list !files) buf;
      Buffer.contents buf

    let bundle_create t ~basis =
      I.Repo.export t.repo.r ~min:basis ~max:[t.commit] >|= fun slice ->
      let b = Buffer.create 10240 in
      let encoder = Jsonm.encoder ~minify:true (`Buffer b) in
      Irmin.Type.encode_json bundle_t encoder (slice, I.Commit.hash t.commit);
      ignore @@ Jsonm.encode encoder `End;
      Some (Buffer.contents b)

    let bundle ~tracking_branch t =
      let equal c1 c2 =
        Cstruct.equal
          (I.Commit.Hash.to_raw (I.Commit.hash c1))
          (I.Commit.Hash.to_raw (I.Commit.hash c2))
      in
      I.of_branch t.repo.r tracking_branch >>= fun tracking_branch ->
      I.Head.find tracking_branch >>= function
      | Some old_head when equal old_head t.commit -> Lwt.return_none
      | Some old_head -> bundle_create t ~basis:[old_head]
      | None -> bundle_create t ~basis:[]

    let parents t =
      I.Commit.parents t.commit >|= List.map (fun commit -> {t with commit})

    let info t =
      I.Commit.info t.commit

    let lcas t other =
      I.of_commit t.commit >>= fun tmp ->
      I.lcas_with_commit tmp other.commit >|= function
      | Ok ids -> ids |> List.map (fun commit -> { t with commit })
      | Error (`Max_depth_reached |`Too_many_lcas) -> assert false  (* Can't happen *)
  end

  module Branch = struct
    type t = {
      repo : repo;
      store : I.t;
      head_id : Commit.id option ref;
      head : Commit.t option React.S.t;
      watch : I.watch;
    }

    let opt_commit_equal a b =
      match a, b with
      | Some a, Some b -> Commit.equal a b
      | None, None -> true
      | _ -> false

    let of_store ?if_new repo store =
      I.Head.find store >>= (function
          | Some commit -> Lwt.return_some (Commit.v repo commit)
          | None ->
            match if_new with
            | None -> Lwt.return_none
            | Some (lazy if_new) ->
              if_new >>= fun commit ->
              I.Head.test_and_set store ~test:None ~set:(Some commit.Commit.commit) >>= function
              | true -> Lwt.return_some commit
              | false ->
                Printf.eprintf "Warning: Concurrent attempt to initialise new branch; discarding our attempt\n%!";
                I.Head.get store >|= fun commit -> Some (Commit.v repo commit)
      ) >>= fun initial_head ->
      let initial_head_commit = initial_head |> option_map (fun x -> x.Commit.commit) in
      let initial_head_id = initial_head |> option_map Commit.id in
      let head_id = ref initial_head_id in
      let head, set_head = React.S.create ~eq:opt_commit_equal initial_head in
      I.watch store ?init:initial_head_commit (fun _diff ->
        (* (ignore the commit ID in the update message; we want the latest) *)
        I.Head.find store >|= fun new_head ->
        let new_head_id = option_map I.Commit.hash new_head in
        if new_head_id <> !head_id then (
          head_id := new_head_id;
          new_head |> option_map (Commit.v repo) |> set_head
        )
      ) >>= fun watch ->
      Lwt.return {
        repo;
        store;
        head_id;
        head;
        watch;
      }

    let head t = t.head

    let fast_forward_to t commit =
      I.Head.fast_forward t.store commit.Commit.commit >|= function
      | Ok () | Error `No_change -> `Ok
      | Error `Rejected -> `Not_fast_forward  (* (concurrent update) *)
      | Error (`Max_depth_reached | `Too_many_lcas) ->
        (* These shouldn't happen, because we didn't set any limits *)
        assert false

    let force t = function
      | Some commit -> I.Head.set t.store commit.Commit.commit
      | None ->
          match I.status t.store with
          | `Empty | `Commit _ -> assert false
          | `Branch branch_name -> I.Branch.remove t.repo.r branch_name

    let fetch_bundle tracking_branch bundle =
      let repo = tracking_branch.repo in
      let decoder = Jsonm.decoder (`String bundle) in
      match Irmin.Type.decode_json bundle_t decoder with
      | Error (`Msg m) -> Lwt.return (`Error (Fmt.strf "Failed to decode slice JSON: %s" m))
      | Ok (slice, head) ->
        (* Check whether we already have [head]. *)
        begin
          I.Commit.of_hash repo.r head >>= function
          | Some commit -> Lwt.return_ok commit
          | None ->
            (* If not, import the slice *)
            I.Repo.import tracking_branch.repo.r slice >>= function
            | Error (`Msg m) -> Lwt_result.fail (Fmt.strf "Failed to import slice: %s" m)
            | Ok () ->
              I.Commit.of_hash repo.r head >>= function
              | Some commit -> Lwt_result.return commit
              | None -> Lwt_result.fail "Head commit not found after importing bundle!"
        end >>= function
        | Error m -> Lwt.return (`Error m)
        | Ok head ->
          I.Head.set tracking_branch.store head >|= fun () ->
          `Ok (Commit.v repo head)

    let release t =
      I.unwatch t.watch
  end

  module Repository = struct
    type t = repo

    let branch t ?if_new name =
      I.of_branch t.r name >>= Branch.of_store ?if_new t

    let branch_head t branch =
      I.Branch.find t.r branch >|= option_map I.Commit.hash

    let commit t id =
      I.Commit.of_hash t.r id >|= option_map (Commit.v t)

    let empty t =
      Staging.create t ~parents:[] ~tree:I.Tree.empty
  end

  let make r info_maker = {r; info_maker}
end
