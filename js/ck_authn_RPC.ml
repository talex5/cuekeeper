(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

(* RPC over XMLHttpRequest, getting the access token from the user if necessary. *)

open Lwt.Infix
open Js_of_ocaml

module XHR = Cohttp_lwt_xhr.Client

let storage =
  lazy (
    Js.Optdef.get (Dom_html.window##.localStorage)
      (fun () -> failwith "HTML 5 storage is not available")
  )

let token_key = Js.string "CueKeeper.auth.access_token"

let set_token value = (Lazy.force storage)##setItem token_key value
let remove_token () = (Lazy.force storage)##removeItem token_key
let get_token () =
  let v = (Lazy.force storage)##getItem(token_key) in
  Js.Opt.map v Js.to_string
  |> Js.Opt.to_option

let input_token () =
  Js.Opt.case (Dom_html.window##prompt (Js.string "Enter access token") (Js.string ""))
    (fun () -> `Cancelled_by_user)
    (fun s -> set_token s; `Ok)

let rec with_token fn uri =
  match get_token () with
  | None ->
      begin match input_token () with
      | `Ok -> with_token fn uri
      | `Cancelled_by_user as c -> Lwt.return c
      end
  | Some access_token ->
      let uri = Uri.add_query_param uri ("token", [access_token]) in
      fn uri >>= function
      | (resp, _body) when resp.Cohttp.Response.status = `Unauthorized ->
          remove_token ();
          with_token fn uri
      | result -> Lwt.return (`Ok result)

let get ?headers uri = with_token (XHR.get ?headers) uri
let post ?body ?headers uri = with_token (XHR.post ?body ?headers) uri
