(* Copyright (C) 2015, Thomas Leonard.
 * See the README file for details. *)

open Lwt

module Make (I : Irmin.BASIC with type key = string list and type value = string) = struct
  module V = Irmin.View(I)
  module Top = Graph.Topological.Make(I.History)

  type repo = {
    config : Irmin.config;
    task_maker : string -> Irmin.task;
    empty : string -> I.t;
  }

  module Staging = struct
    type t = {
      repo : repo;
      view : V.t;
    }

    let of_view repo view = {repo; view}
    let list t = V.list t.view
    let read_exn t = V.read_exn t.view
    let update t = V.update t.view
    let remove t = V.remove t.view
    let mem t = V.mem t.view
  end

  module Commit = struct
    type t = {
      repo : repo;
      store : string -> I.t;
    }

    type id = Irmin.Hash.SHA1.t

    module History = I.History

    let id t =
      match I.branch (t.store "get commit ID") with
      | `Tag _ | `Empty -> assert false
      | `Head id -> id

    let equal a b =
      id a = id b

    let of_id repo id =
      I.of_head repo.config repo.task_maker id >|= fun store ->
      {repo; store}

    let checkout t =
      V.of_path (t.store "Make view") I.Key.empty >|= Staging.of_view t.repo

    let commit ?parents staging ~msg =
      let repo = staging.Staging.repo in
      let parents =
        match parents with
        | Some parents -> parents |> List.map id
        | None -> V.parents staging.Staging.view in
      V.make_head (repo.empty msg) (repo.task_maker msg) ~parents ~contents:staging.Staging.view
      >>= I.of_head repo.config repo.task_maker
      >|= fun store -> { repo; store }

    let history ?depth t =
      let open Git_storage_s in
      I.history ?depth (t.store "Read history") >>= fun history ->
      let h = ref [] in
      history |> Top.iter (fun head ->
        h := head :: !h
      );
      let map = ref Log_entry_map.empty in
      let rank = ref 0 in
      !h |> Lwt_list.iter_s (fun hash ->
          I.task_of_head (t.store "Read log entry") hash >|= fun task ->
          incr rank;
          let msg = Irmin.Task.messages task in
          let date = Irmin.Task.date task |> Int64.to_float in
          let entry = {Log_entry.date; rank = !rank; msg; id = hash} in
          map := !map |> Log_entry_map.add entry entry
      ) >|= fun () ->
      !map

    let merge a b =
      I.of_head a.repo.config a.repo.task_maker (id a) >>= fun tmp ->
      I.merge_head (tmp "Merge") (id b) >|= function
      | `Ok () -> `Ok {a with store = tmp}
      | `Conflict _ as c -> c
  end

  module Branch = struct
    type t = {
      repo : repo;
      store : string -> I.t;
      head_id : Commit.id option ref;
      head : Commit.t option React.S.t;
    }

    let opt_commit_equal a b =
      match a, b with
      | Some a, Some b -> Commit.equal a b
      | None, None -> true
      | _ -> false

    let of_store repo store ~if_new =
      match I.branch (store "Get branch name") with
      | `Head _ | `Empty -> failwith "Not a tag!"
      | `Tag branch_name ->
      let commit_of_id = function
        | None -> return None
        | Some id -> Commit.of_id repo id >|= fun commit -> Some commit in
      (* Start watching for updates (must do this BEFORE getting the initial commit) *)
      let watch_tags = I.watch_tags (store "Watch branch") in
      I.head (store "Get latest commit") >>= (function
        | Some id -> return id
        | None ->
            Lazy.force if_new >>= fun commit ->
            let new_id = Commit.id commit in
            I.compare_and_set_head (store "Initialise repository") ~test:None ~set:(Some new_id) >>= function
            | true -> return new_id
            | false ->
                Log.warn "Concurrent attempt to initialise new branch; discarding our attempt";
                I.head_exn (store "Read new head")
      ) >>= fun initial_head_id ->
      let head_id = ref (Some initial_head_id) in
      Commit.of_id repo initial_head_id >>= fun initial_head ->
      let head, set_head = React.S.create ~eq:opt_commit_equal (Some initial_head) in
      async (fun () ->
        watch_tags |> Lwt_stream.iter_s (function
          | (n, Some _id) when n = branch_name ->
              (* (ignore the commit ID in the update message; we want the latest) *)
              I.head (store "Get latest commit") >>= fun new_head_id ->
              if new_head_id <> !head_id then (
                head_id := new_head_id;
                commit_of_id new_head_id >|= set_head
              ) else return ()
          | _ -> return ()
        )
      );
      return {
        repo;
        store;
        head_id;
        head;
      }

    let head t = t.head

    let fast_forward_to t commit =
      (* Note: can't use [I.fast_forward_head] because it can sometimes return [false] on success
       * (when already up-to-date). *)
      let store = t.store "Fast-forward" in
      let commit_id = Commit.id commit in
      let old_head = !(t.head_id) in
      let do_ff () =
        I.compare_and_set_head store ~test:old_head ~set:(Some commit_id) >|= function
        | true -> `Ok
        | false -> `Not_fast_forward in   (* (concurrent update) *)
      match old_head with
      | None -> do_ff ()
      | Some expected ->
          I.lcas_head store commit_id >>= function
          | `Ok lcas ->
              if List.mem expected lcas then do_ff ()
              else return `Not_fast_forward
          (* These shouldn't happen, because we didn't set any limits *)
          | `Max_depth_reached | `Too_many_lcas -> assert false
  end

  module Repository = struct
    type t = repo

    let branch t ~if_new name =
      I.of_tag t.config t.task_maker name >>= Branch.of_store t ~if_new

    let commit t hash =
      (* XXX: what does Irmin do if the hash doesn't exist? *)
      Commit.of_id t hash >|= fun c -> Some c

    let empty t = V.empty () >|= Staging.of_view t
  end

  let make config task_maker =
    I.empty config task_maker >|= fun empty ->
    {config; task_maker; empty}
end
