open Lwt

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

module Git = Git_storage.Make(Irmin.Basic(Html_storage.Make)(Irmin.Contents.String))
module M = Ck_model.Make(Clock)(Git)(Ck_template.Gui_tree_data)
module T = Ck_template.Make(M)

let start (main:#Dom.node Js.t) =
  let config = Html_storage.config "CueKeeper" in
  let task s =
    let date = Unix.time () |> Int64.of_float in
    Irmin.Task.create ~date ~owner:"User" s in
  let repo = Git.make config task in
  M.make repo >>= fun m ->
  T.make_top m
  |> List.iter (fun child -> main##appendChild (Tyxml_js.To_dom.of_node child) |> ignore);
  return ()

let () =
  match Dom_html.tagged (Dom_html.getElementById "ck_main") with
  | Dom_html.Div d -> Lwt_js_events.async (fun () -> start d)
  | _ -> failwith "Bad tree element"
