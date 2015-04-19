open Lwt
open Ck_utils

(* let () = Log.(set_log_level INFO) *)

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

module Git = Git_storage.Make(Irmin.Basic(Irmin_IDB.Make)(Irmin.Contents.String))
module M = Ck_model.Make(Clock)(Git)(Ck_template.Gui_tree_data)
module T = Ck_template.Make(M)

let start (main:#Dom.node Js.t) =
  Lwt.catch
    (fun () ->
      let config = Irmin_IDB.config "CueKeeper" in
      let task s =
        let date = Unix.time () |> Int64.of_float in
        Irmin.Task.create ~date ~owner:"User" s in
      Git.make config task >>= M.make >>= fun m ->
      let icon =
        let open Tyxml_js in
        let href = M.alert m >|~= (function
          | false -> "resources/ico/ck.ico"
          | true -> "resources/ico/ck-alert.ico"
        ) in
        R.Html5.link ~rel:(React.S.const [`Icon]) ~href ~a:[Html5.a_mime_type "image/ico"] () in
      Dom_html.document##head##appendChild (Tyxml_js.To_dom.of_node icon) |> ignore;
      T.make_top m
      |> List.iter (fun child -> main##appendChild (Tyxml_js.To_dom.of_node child) |> ignore);
      return ()
    )
    (fun ex ->
      let open Tyxml_js.Html5 in
      let error = div ~a:[a_class ["alert-box"; "alert"]] [pcdata (Printexc.to_string ex)] in
      main##appendChild (Tyxml_js.To_dom.of_node error) |> ignore;
      raise ex
    )

let () =
  match Dom_html.tagged (Dom_html.getElementById "ck_main") with
  | Dom_html.Div d -> Lwt_js_events.async (fun () -> start d)
  | _ -> failwith "Bad tree element"
