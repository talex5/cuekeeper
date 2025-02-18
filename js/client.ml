(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Lwt.Infix
open Ck_utils
open Js_of_ocaml
open Js_of_ocaml_tyxml

module Lwt_js = Js_of_ocaml_lwt.Lwt_js
module Lwt_js_events = Js_of_ocaml_lwt.Lwt_js_events

(* let () = Log.(set_log_level INFO) *)

let () = Random.self_init ()    (* Random back-off for Safari bug work-around *)

module Clock = struct
  let now = Unix.gettimeofday
  let async ~name fn =
    Lwt_js_events.async (fun () ->
      Lwt.catch fn (fun ex ->
        Printf.printf "Async error in '%s'" name;
        Lwt.fail ex
      )
    )
  let sleep = Lwt_js.sleep
end

module Store = Irmin_git.Generic(Irmin_indexeddb.Content_store_git)(Irmin_indexeddb.Branch_store)
    (Irmin.Contents.String)(Irmin.Path.String_list)(Irmin.Branch.String)
module Git = Git_storage.Make(Store)
module M = Ck_model.Make(Clock)(Git)(Ck_template.Gui_tree_data)(Ck_authn_RPC)
module T = Ck_template.Make(M)

let server =
  let use_server = Js.Unsafe.pure_js_expr "ck_use_server" |> Js.to_bool in
  if use_server then Some (
    Js.to_string Dom_html.window##.location##.protocol ^ Js.to_string Dom_html.window##.location##.host
    |> Uri.of_string
  ) else None

let start (main:#Dom.node Js.t) =
  Lwt.catch
    (fun () ->
      let config = Irmin_indexeddb.config "CueKeeper" in
      let task s =
        let date = Unix.time () |> Int64.of_float in
        Irmin.Info.v ~date ~author:"User" s in
      Store.Repo.v config >>= fun repo ->
      M.make ?server (Git.make repo task) >>= fun m ->
      let icon =
        let open Tyxml_js in
        let href = M.alert m >|~= (function
          | false -> "resources/ico/ck.ico"
          | true -> "resources/ico/ck-alert.ico"
        ) in
        R.Html5.link ~rel:(React.S.const [`Icon]) ~href ~a:[Html5.a_mime_type "image/ico"] () in
      Dom_html.document##.head##appendChild (Tyxml_js.To_dom.of_node icon) |> ignore;
      T.make_top m
      |> List.iter (fun child -> main##appendChild (Tyxml_js.To_dom.of_node child) |> ignore);
      Lwt.return ()
    )
    (fun ex ->
      let msg =
        match ex with
        | Irmin_indexeddb.Format_too_old `Irmin_0_10 ->
          "Please upgrade to CueKeeper 0.3 first. This will convert your old data to the standard Git format, \
            which is the only format the current version of CueKeeper can read."
        | _ ->
          let msg = Printexc.to_string ex in
          if Regexp.string_match (Regexp.regexp_string "SecurityError:") msg 0 <> None then
            msg ^ " Ensure cookies are enabled (needed to access local storage)."
          else msg
      in
      let error = Tyxml_js.Html5.(div ~a:[a_class ["alert-box"; "alert"]]
                                    [txt msg]) in
      main##appendChild (Tyxml_js.To_dom.of_node error) |> ignore;
      raise ex
    )

let () =
  match Dom_html.tagged (Dom_html.getElementById "ck_main") with
  | Dom_html.Div d -> Lwt_js_events.async (fun () -> start d)
  | _ -> failwith "Bad tree element"
