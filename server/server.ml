(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Lwt.Infix

let src = Logs.Src.create "cuekeeper" ~doc:"CueKeeper server"
module Log = (val Logs.src_log src : Logs.LOG)

let load path =
  let ch = open_in_bin path in
  let len = in_channel_length ch in
  let data = really_input_string ch len in
  close_in ch;
  data

let resource_paths = [
  "index.html";
  "resources/cache.manifest";
  "resources/css/foundation.css";
  "resources/css/foundation.min.css";
  "resources/css/normalize.css";
  "resources/css/pikaday.css";
  "resources/css/style.css";
  "resources/ico/ck-alert.ico";
  "resources/ico/ck.ico";
  "resources/js/cuekeeper.js";
  "resources/js/vendor";
  "resources/js/vendor/FileSaver.min.js";
  "resources/js/vendor/pikaday.js";
]

module Make (Store:Irmin.S with type branch = string and type hash = Digestif.SHA1.t) (S:Cohttp_lwt.S.Server) = struct
  type t = {
    static_files : string;
  }

  let make ~static_files = { static_files }

  let bundle_t = Irmin.Type.pair Store.Private.Slice.t Store.Hash.t

  let show_head = function
    | None -> "(none)"
    | Some head -> String.sub (Fmt.to_to_string Store.Commit.pp_hash head) 0 6

  let respond_static t segments =
    let path = String.concat "/" segments in
    if List.mem path resource_paths then (
      let body = load (Filename.concat t.static_files path) in
      let headers =
        if Filename.check_suffix path ".html" then
          Cohttp.Header.init_with "Content-Type" "text/html"
        else if Filename.check_suffix path ".manifest" then
          Cohttp.Header.init_with "Content-Type" "text/cache-manifest"
        else
          Cohttp.Header.init () in
      S.respond_string ~headers ~status:`OK ~body ()
    ) else S.respond_not_found ()

  (* Split a URI into a list of path segments *)
  let split_path uri =
    let path = Uri.path uri in
    let rec aux = function
      | [] | [""] -> []
      | hd::tl -> hd :: aux tl
    in
    List.filter (fun e -> e <> "")
      (aux (Re.Str.(split_delim (regexp_string "/") path)))

  let hash_equal = Irmin.Type.equal Store.Hash.t

  (* Import bundle from client into store. *)
  let accept_push s body =
    let headers = Cohttp.Header.init_with "Content-Type" "application/octet-stream" in
    match s () with
    | Error resp -> resp
    | Ok s ->
      Cohttp_lwt.Body.to_string body >>= fun body ->
      let decoder = Jsonm.decoder (`String body) in
      match Irmin.Type.decode_json bundle_t decoder with
      | Error (`Msg m) ->
        let msg = Fmt.str "Failed to decode slice JSON: %s" m in
        Log.warn (fun f -> f "%s" msg);
        S.respond_string ~headers ~status:`Bad_request ~body:msg ()
      | Ok (slice, head) ->
        Store.Head.find s >>= function
        | Some server_head when hash_equal (Store.Commit.hash server_head) head ->
          S.respond_string ~headers ~status:`OK ~body:"ok" ()
        | server_head ->
          Store.Repo.import (Store.repo s) slice >>= function
          | Error (`Msg m) ->
            let msg = Fmt.str "Failed to import slice: %s" m in
            Log.warn (fun f -> f "%s" msg);
            S.respond_string ~headers ~status:`Bad_request ~body:msg ()
          | Ok () ->
            Store.Commit.of_hash (Store.repo s) head >>= function
            | None ->
              let msg = "New head not found after import!" in
              Log.warn (fun f -> f "%s" msg);
              S.respond_string ~headers ~status:`Bad_request ~body:msg ()
            | Some head ->
              Store.Head.fast_forward s head >>= function
              | Error (`Max_depth_reached | `Too_many_lcas) -> assert false
              | Error `Rejected ->
                let msg = Printf.sprintf "Non-fast-forward push attempted: %s -> %s"
                    (show_head server_head) (show_head (Some head)) in
                Log.warn (fun f -> f "%s" msg);
                S.respond_string ~headers ~status:`OK ~body:"not-fast-forward" ()
              | Ok () | Error `No_change ->
                Log.info (fun f -> f "Update master %s -> %s" (show_head server_head) (show_head (Some head)));
                S.respond_string ~headers ~status:`OK ~body:"ok" ()

  (* Export changes in the store since [last_known] to a bundle for the client. *)
  let fetch s last_known =
    let headers = Cohttp.Header.init_with "Content-Type" "application/octet-stream" in
    match s () with
    | Error resp -> resp
    | Ok s ->
      Store.Head.find s >>= function
      | None -> S.respond_string ~headers ~status:`OK ~body:"" ()
      | Some head ->
        begin match last_known with
          | None -> Lwt.return []
          | Some c ->
            Store.Commit.of_hash (Store.repo s) c >|= function
            | None -> []
            | Some c -> [c]
        end >>= fun basis ->
        Store.Repo.export (Store.repo s) ~min:basis ~max:(`Max [head]) >>= fun slice ->
        let b = Buffer.create 10240 in
        let encoder = Jsonm.encoder ~minify:true (`Buffer b) in
        Irmin.Type.encode_json bundle_t encoder (slice, Store.Commit.hash head);
        ignore @@ Jsonm.encode encoder `End;
        let body = Buffer.contents b in
        S.respond_string ~headers ~status:`OK ~body ()

  let handle_request t s (_io_conn, http_conn) request body =
    Lwt.catch (fun () ->
        let uri = Cohttp.Request.uri request in
        Log.info (fun f -> f "%s: %s %s"
                     (Cohttp.Connection.to_string http_conn)
                     (Cohttp.Request.meth request |> Cohttp.Code.string_of_method)
                     (Uri.to_string uri));
        match Cohttp.Request.meth request, split_path uri with
        | `GET, ["fetch"] -> fetch s None
        | `GET, ["fetch"; last_known] ->
          begin match Irmin.Type.of_string Store.Hash.t last_known with
            | Ok hash -> fetch s (Some hash)
            | Error (`Msg m) ->
              Log.warn (fun f -> f "Invalid hash %S: %s" last_known m);
              S.respond_error ~status:`Bad_request ~body:"Invalid hash" ()
          end
        | `POST, ["push"] -> accept_push s body
        | `GET, ([] | [""]) -> respond_static t ["index.html"]
        | `GET, segments -> respond_static t segments
        | _ -> S.respond_error ~status:`Method_not_allowed ~body:"Invalid request" ()
      ) (fun ex ->
        Log.warn (fun f -> f "Unhandled exception processing HTTP request: %s" (Printexc.to_string ex));
        Lwt.fail ex
      )
end
