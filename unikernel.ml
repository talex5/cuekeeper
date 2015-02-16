(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Lwt
open Ck_utils

let html_decl = "\
<!DOCTYPE html>
<!--[if IE 9]><html class=\"lt-ie10\" lang=\"en\" > <![endif]-->"

(* Split a URI into a list of path segments (from mirage-skeleton) *)
let split_path path =
  let rec aux = function
    | [] | [""] -> []
    | hd::tl -> hd :: aux tl
  in
  List.filter (fun e -> e <> "")
    (aux (Re_str.(split_delim (regexp_string "/") path)))

let string_of_html doc =
  let b = Buffer.create 1000 in
  Buffer.add_string b html_decl;
  Cow.Html.output_doc (`Buffer b) doc;
  Buffer.contents b

module Main (C : V1_LWT.CONSOLE) (H : Cohttp_lwt.Server) (R : V1_LWT.KV_RO) = struct
  let unsupported_method = H.respond_error ~status:`Bad_request ~body:"Method not supported\n" ()

  let handle_main m _request =
    let body =
      Ck_template.render_main m
      |> string_of_html in
    H.respond_string ~status:`OK ~body ()

  (* (from mirage-skeleton) *)
  let read fs name =
    R.size fs name
    >>= function
      | `Error (R.Unknown_key _) -> fail (Failure ("read " ^ name))
      | `Ok size ->
          R.read fs name 0 (Int64.to_int size)
          >>= function
            | `Error (R.Unknown_key _) -> fail (Failure ("read " ^ name))
            | `Ok bufs -> return (Cstruct.copyv bufs)

  let handle_add m body =
    Cohttp_lwt_body.to_string body >>= fun body ->
    let params = Uri.query_of_encoded body in
    params |> List.iter (fun (k, v) -> Printf.printf "%s=%S\n%!" k (String.concat ":" v));
    let get name =
      try
        match List.assoc name params with
        | [value] -> value
        | [] -> error "Missing '%s'" name
        | _ -> error "Multiple values for '%s'" name
      with Not_found -> error "Missing '%s'" name in
    let maker =
      match get "type" with
      | "Action" -> Ck_irmin_model.add_action
      | "Project" -> Ck_irmin_model.add_project
      | "Area" -> Ck_irmin_model.add_area
      | x -> error "Unknown node type '%s'" x in
    maker m
      ~name:(get "name")
      ~description:(get "description")
      ~parent:(get "parent")
    >>= fun () ->
    H.respond_redirect ~uri:(Uri.of_string "/") ()

  let start c http r =
    Ck_logger.write := C.log_s c;
    Ck_logger.info "start cuekeeper service" >>= fun () ->

    let config = Irmin_git.config ~root:"db" ~bare:true () in
    Ck_irmin_model.Store.create config Irmin_unix.task >>= fun t ->
    Ck_irmin_model.make t >>= fun m ->

    let callback _conn_id request body =
      try_lwt
        let path = Uri.path request.Cohttp.Request.uri in
        match H.Request.meth request, split_path path with
        | `GET, [] -> handle_main m request
        | `POST, ["add"] -> handle_add m body
        | `GET, ("js" | "css") :: _ ->
            read r path >>= fun body -> H.respond_string ~status:`OK ~body ()
        | _ -> H.respond_error ~status:`Bad_request ~body:(Printf.sprintf "Bad path '%s'\n" path) ()
      with ex ->
        Ck_logger.warn "error handling HTTP request: %s\n%s"
          (Printexc.to_string ex)
          (Printexc.get_backtrace ()) >>= fun () ->
        raise ex in

    let conn_closed _conn_id =
      Ck_logger.info "connection closed" |> ignore in

    http (H.make ~callback ~conn_closed ())
end

