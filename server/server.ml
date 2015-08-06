(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Lwt

let show_head = function
  | None -> "(none)"
  | Some head -> String.sub (Irmin.Hash.SHA1.to_hum head) 0 6

module Make (Store:Irmin.BASIC) (S:Cohttp_lwt.Server) = struct
  module Bundle = Tc.Pair(Store.Private.Slice)(Store.Head)

  let respond_static segments =
    let path = String.concat "/" segments in
    match Static.read path with
    | None -> S.respond_not_found ()
    | Some body ->
        let headers =
          if Filename.check_suffix path ".html" then
            Cohttp.Header.init_with "Content-Type" "text/html"
          else if Filename.check_suffix path ".manifest" then
            Cohttp.Header.init_with "Content-Type" "text/cache-manifest"
          else
            Cohttp.Header.init () in
        S.respond_string ~headers ~status:`OK ~body ()

  (* Split a URI into a list of path segments *)
  let split_path uri =
    let path = Uri.path uri in
    let rec aux = function
      | [] | [""] -> []
      | hd::tl -> hd :: aux tl
    in
    List.filter (fun e -> e <> "")
      (aux (Re_str.(split_delim (regexp_string "/") path)))

  (* Import bundle from client into store. *)
  let accept_push s body =
    let headers = Cohttp.Header.init_with "Content-Type" "application/octet-stream" in
    match s "import" with
    | `Error resp -> resp
    | `Ok s ->
    Cohttp_lwt_body.to_string body >>= fun body ->
    let buf = Mstruct.of_string (B64.decode body) in
    let (slice, head) = Bundle.read buf in
    Store.head s >>= function
    | Some server_head when server_head = head ->
        S.respond_string ~headers ~status:`OK ~body:"ok" ()
    | server_head ->
    Store.import s slice >>= function
    | `Error ->
        let msg = "Failed to import slice" in
        Log.warn "%s" msg;
        S.respond_string ~headers ~status:`Bad_request ~body:msg ()
    | `Ok ->
    let commit_store = Store.Private.commit_t s in
    Store.Private.Commit.mem commit_store head >>= function
    | false ->
        let msg = "New head not found after import!" in
        Log.warn "%s" msg;
        S.respond_string ~headers ~status:`Bad_request ~body:msg ()
    | true ->
    Store.fast_forward_head s head >>= function
    | false ->
        let msg = Printf.sprintf "Non-fast-forward push attempted: %s -> %s"
          (show_head server_head) (show_head (Some head)) in
        Log.warn "%s" msg;
        S.respond_string ~headers ~status:`OK ~body:"not-fast-forward" ()
    | true ->
        Log.info "Update master %s -> %s" (show_head server_head) (show_head (Some head));
        S.respond_string ~headers ~status:`OK ~body:"ok" ()

  (* Export changes in the store since [last_known] to a bundle for the client. *)
  let fetch s last_known =
    let headers = Cohttp.Header.init_with "Content-Type" "application/octet-stream" in
    match s "export" with
    | `Error resp -> resp
    | `Ok s ->
    Store.head s >>= function
    | None -> S.respond_string ~headers ~status:`OK ~body:"" ()
    | Some head ->
    let basis =
      match last_known with
      | None -> []
      | Some c -> [Irmin.Hash.SHA1.of_hum c] in
    Store.export s ~min:basis ~max:[head] >>= fun slice ->
    let bundle = (slice, head) in
    let buf = Cstruct.create (Bundle.size_of bundle) in
    let rest = Bundle.write bundle buf in
    assert (Cstruct.len rest = 0);
    let body = Cstruct.to_string buf |> B64.encode in
    S.respond_string ~headers ~status:`OK ~body ()

  let handle_request s (_io_conn, http_conn) request body =
    Lwt.catch (fun () ->
      let uri = Cohttp.Request.uri request in
      Log.info "%s: %s %s"
        (Cohttp.Connection.to_string http_conn)
        (Cohttp.Request.meth request |> Cohttp.Code.string_of_method)
        (Uri.to_string uri);
      match Cohttp.Request.meth request, split_path uri with
      | `GET, ["fetch"] -> fetch s None
      | `GET, ["fetch"; last_known] -> fetch s (Some last_known)
      | `POST, ["push"] -> accept_push s body
      | `GET, ([] | [""]) -> respond_static ["index.html"]
      | `GET, segments -> respond_static segments
      | _ -> S.respond_error ~status:`Method_not_allowed ~body:"Invalid request" ()
    ) (fun ex ->
      Log.warn "Unhandled exception processing HTTP request: %s" (Printexc.to_string ex);
      fail ex
    )
end
