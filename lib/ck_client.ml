(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Lwt
open Ck_utils
open Ck_sigs

let tracking_branch = "server"

module Make(Clock : Ck_clock.S)
           (Git : Git_storage_s.S)
           (RPC : Cohttp_lwt.Client) = struct
  type t = {
    master : Git.Branch.t;
    server_branch : Git.Branch.t;
    merge_from : Git.Commit.t -> unit or_error Lwt.t;
    base : Uri.t;
    sync_in_progress : bool React.S.t;
    set_sync_in_progress : bool -> unit;
  }

  let make ~master ~server_branch ~merge_from base =
    let sync_in_progress, set_sync_in_progress = React.S.create false in
    {
      master; server_branch; merge_from; base;
      sync_in_progress; set_sync_in_progress;
    }

  let get ~base path =
    RPC.get (Uri.with_path base path) >>= fun (resp, body) ->
    match resp.Cohttp.Response.status with
    | `OK -> Cohttp_lwt_body.to_string body >|= fun body -> `Ok body
    | code -> return (error "Bad status code '%s' from server" (Cohttp.Code.string_of_status code))

  let post ~base path body =
    let body = Cohttp_lwt_body.of_string (B64.encode body) in
    let headers = Cohttp.Header.init_with "Content-Type" "application/octet-stream" in
    RPC.post ~headers ~body (Uri.with_path base path) >>= fun (resp, body) ->
    match resp.Cohttp.Response.status with
    | `OK -> Cohttp_lwt_body.to_string body >|= fun body -> `Ok body
    | code -> return (error "Bad status code '%s' from server" (Cohttp.Code.string_of_status code))

  let fetch ~base ~server_branch =
    let path =
      match React.S.value (Git.Branch.head server_branch) with
      | Some last_known -> "fetch/" ^ Irmin.Hash.SHA1.to_hum (Git.Commit.id last_known)
      | None -> "fetch" in
    get ~base path >>!= function
    | "" ->
        Git.Branch.force server_branch None >|= fun () -> `Ok None
    | bundle ->
        Git.Branch.fetch_bundle server_branch (B64.decode bundle) >>!= fun commit ->
        return (`Ok (Some commit))

  let pull t  =
    fetch ~base:t.base ~server_branch:t.server_branch >>!= function
    | None -> return (`Ok None)
    | Some commit ->
    (* If server_head isn't in the history of master, merge it now. *)
    t.merge_from commit >>!= fun () -> return (`Ok (Some commit))

  let push t server_head =
    let new_head =
      match Git.Branch.head t.master |> React.S.value with
      | Some commit -> commit
      | None -> failwith "no master branch!" in
    if Some new_head = server_head then return (`Ok ())
    else (
      Git.Commit.bundle ~tracking_branch new_head >>= function
      | None -> return (`Ok ())
      | Some bundle ->
      post ~base:t.base "push" bundle >>!= function
      | "ok" ->
          begin Git.Branch.fast_forward_to t.server_branch new_head >>= function
          | `Not_fast_forward ->
              return (error "Push successful, but failed to fast-forward tracking branch - newer concurrent push?")
          | `Ok -> return (`Ok ())
          end
      | "not-fast-forward" -> return `Concurrent_update
      | msg -> return (error "Unexpected response '%s'" msg)
    )

  let sync t =
    let rec aux () =
      pull t >>!= fun server_head ->
      (* Our master branch now includes [server_head] *)
      push t server_head >>= function
      | `Concurrent_update ->
          Log.warn "Concurrent update during sync; retrying";
          Clock.sleep 2.0 >>= aux
      | `Ok () | `Error _ as r -> return r in
    if React.S.value t.sync_in_progress then
      return (`Error "Sync already in progress")
    else (
      t.set_sync_in_progress true;
      finalize aux (fun () -> t.set_sync_in_progress false; return ())
    )

  let sync_in_progress t = t.sync_in_progress
end
