(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

(* RPC over XMLHttpRequest, getting the access token from the user if necessary. *)

open Lwt

module XHR = Cohttp_lwt_xhr.Client

let token = ref None

let get_token () =
  Js.Opt.case (Dom_html.window##prompt(Js.string "Enter access token", Js.string ""))
    (fun () -> `Cancelled_by_user)
    (fun s -> token := Some (Js.to_string s); `Ok)

let rec with_token fn uri =
  match !token with
  | None ->
      begin match get_token() with
      | `Ok -> with_token fn uri
      | `Cancelled_by_user as c -> return c
      end
  | Some access_token ->
      let uri = Uri.add_query_param uri ("token", [access_token]) in
      fn uri >>= function
      | (resp, _body) when resp.Cohttp.Response.status = `Unauthorized -> token := None; with_token fn uri
      | result -> return (`Ok result)

let get ?headers uri = with_token (XHR.get ?headers) uri
let post ?body ?headers uri = with_token (XHR.post ?body ?headers) uri
