(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

(* RPC over XMLHttpRequest, getting the access token from the user if necessary. *)

open Lwt

include Cohttp_lwt_xhr.Client

let token = ref None

let get_token () =
  token :=
    Js.Opt.map (Dom_html.window##prompt(Js.string "Enter access token", Js.string "")) Js.to_string
    |> Js.Opt.to_option

let rec with_token fn uri =
  match !token with
  | None -> get_token(); with_token fn uri
  | Some token ->
      let uri = Uri.add_query_param uri ("token", [token]) in
      fn uri >>= function
      | (resp, _body) when resp.Cohttp.Response.status = `Unauthorized -> get_token(); with_token fn uri
      | result -> return result

let get ?ctx ?headers uri = with_token (get ?ctx ?headers) uri
let post ?ctx ?body ?chunked ?headers uri = with_token (post ?ctx ?body ?chunked ?headers) uri
