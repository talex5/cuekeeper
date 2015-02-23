(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Tyxml_js
open Html5
open Ck_utils

(* (forces return type to be unit) *)
let async : (unit -> unit Lwt.t) -> unit = Lwt_js_events.async

module Delay = Delay_RList.Make(struct
  let now = Unix.gettimeofday
  let sleep = Lwt_js.sleep
end)

(* Get the index of an item in an assoc list. *)
let index_of key items =
  let rec aux i = function
    | [] -> None
    | (k, _v) :: _ when k = key -> Some i
    | _ :: xs -> aux (i + 1) xs in
  aux 0 items

let (>>?=) = Js.Opt.bind

module Make (M : Ck_sigs.MODEL) = struct

  let current_highlight, set_highlight = React.S.create None

  let class_of_node_type = function
    | `Area -> "ck-area"
    | `Project _ -> "ck-project"
    | `Action _ -> "ck-action"
    | `Deleted -> "ck-deleted"

  let class_of_time_and_type ctime node_type =
    let ty = ["ck-title"; class_of_node_type node_type] in
    let lifetime = Unix.gettimeofday () -. ctime in
    if lifetime >= 0.0 && lifetime <  1.0 then "new" :: ty
    else ty

  let toggle_label ~set_state ~current state =
    let l =
      match state with
      | `Done -> "x"
      | `Next -> "n"
      | `Waiting -> "w"
      | `Future -> "f"
      | `Active -> "a"
      | `SomedayMaybe -> "sm" in
    let cl = if current = state then "ck-active-" ^ l else "ck-inactive" in
    let changed _ev =
      if current <> state then set_state state; true in
    a ~a:[a_class [cl]; a_onclick changed] [pcdata l]

  let make_toggles ~set_state current options =
    options |> List.map (toggle_label ~set_state ~current)

  let toggles_for_type m node = function
    | `Action {Ck_sigs.astate = s; _} ->
        let set_state n =
          Lwt_js_events.async (fun () ->
            M.set_state m node.M.uuid (`Action {Ck_sigs.astate = n})
          ) in
        make_toggles ~set_state s [`Done; `Next; `Waiting; `Future]
    | `Project {Ck_sigs.pstate = s; _} ->
        let set_state n =
          Lwt_js_events.async (fun () ->
            M.set_state m node.M.uuid (`Project {Ck_sigs.pstate = n})
          ) in
        make_toggles ~set_state s [`Done; `Active; `SomedayMaybe]
    | `Area | `Deleted -> []

  let make_state_toggles m node =
    let init = React.S.value node.M.node_type |> toggles_for_type m node in
    let toggles = node.M.node_type >|~= toggles_for_type m node in
    rlist_of ~init toggles

  let rec make_node_view m ~show_node {Delay_RList.data = node; state; _} : _ Html5.elt =
    let children = node.M.child_views
      |> Delay.make ~delay:1.0
      |> ReactiveData.RList.map (make_node_view m ~show_node) in
    let clicked _ev = show_node node.M.uuid; true in
    let delete _ev = async (fun () -> M.delete m node.M.uuid); true in
    let title_cl = React.S.map (class_of_time_and_type node.M.ctime) node.M.node_type in
    let deleted = state >|~= function
      | `Removed -> ["removed"]
      | _ -> [] in
    li ~a:[R.Html5.a_class deleted] [
      R.Html5.span ~a:[a_class ["ck-toggles"]] (make_state_toggles m node);
      a ~a:[R.Html5.a_class title_cl; a_href "#"; a_onclick clicked] [R.Html5.pcdata node.M.name];
      if M.is_root node.M.uuid then pcdata ""
      else a ~a:[a_class ["delete"]; a_onclick delete] [entity "cross"];
      R.Html5.ul children;
    ]

  let make_work_view m ~show_node actions =
    let children = actions
      |> Delay.make ~delay:1.0
      |> ReactiveData.RList.map (make_node_view m ~show_node) in
    [
      h4 [pcdata "Next actions"];
      R.Html5.ul children;
    ]

  let make_sync history =
    let items =
      rlist_of ~init:(React.S.value history) history
      |> ReactiveData.RList.map (fun (date, summary) ->
          let open Unix in
          let tm = gmtime date in
          let msg = Printf.sprintf "%04d-%02d-%02d: %s"
            (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
            summary in
          li [pcdata msg]
      ) in
    [
      h4 [pcdata "Recent changes"];
      R.Html5.ol ~a:[a_class ["ck-history"]] items;
    ]

  let make_tree ~show_node current_mode m =
    let tab mode contents =
      let cl = current_mode |> React.S.map (fun m ->
        if m = mode then ["content"; "active"] else ["content"]
      ) in
      div ~a:[R.Html5.a_class cl] contents in
    let process_tree = M.process_tree m  in
    let process = make_node_view m ~show_node (Delay_RList.fixed process_tree) in
    let work = M.work_tree m |> make_work_view m ~show_node in
    div ~a:[a_class ["tabs-content"]] [
      tab `Process [ul [process]];
      tab `Work work;
      tab `Sync (make_sync (M.history m));
    ]

  let make_mode_switcher current_mode set_current_mode =
    let item name mode =
      let cl = current_mode |> React.S.map (fun m ->
        if m = mode then ["active"] else []
      ) in
      let clicked _ev = set_current_mode mode; true in
      let button = a ~a:[a_href "#"; a_onclick clicked] [pcdata name] in
      dd ~a:[R.Html5.a_class cl] [button] in

    dl ~a:[a_class ["sub-nav"]] [
      item "Process" `Process;
      item "Work" `Work;
      item "Review" `Review;
      item "Contact" `Contact;
      item "Schedule" `Schedule;
      item "Sync" `Sync;
    ]

  let make_child_adder m item =
    let editing, set_editing = React.S.create None in
    let add_button ntype label =
      let start_editing (ev:#Dom_html.event Js.t) =
        (* Find the parent <li> before we remove the button element. *)
        let parent =
          ev##target >>?= fun button ->
          button##parentNode >>?= Dom_html.CoerceTo.element in
        (* Replace the buttons with a form. *)
        set_editing (Some ntype);
        (* Now find the new form input and focus it. *)
        let input =
          parent >>?= fun parent ->
          parent##querySelector (Js.string "input") >>?= Dom_html.CoerceTo.input in
        Js.Opt.iter input (fun i -> i##focus ());
        true in
      a ~a:[a_onclick start_editing] [pcdata label] in
    let widgets =
      editing >>~= (function
        (* When we're not editing, display the add buttons. *)
        | None ->
            item.M.details_type |> React.S.map (function
              | `Deleted -> []
              | `Action _ -> []
              | `Project _ -> [
                  add_button `Action "+action";
                  add_button `Project "+sub-project";
                ]
              | `Area -> [
                  add_button `Project "+project";
                  add_button `Action "+action";
                  add_button `Area "+sub-area";
                ]
            )
        (* When we are editing, display the form. *)
        | Some node_type ->
            let do_add ev =
              let form =
                ev##target >>?= fun target ->
                target##parentNode >>?= fun parent ->
                Dom_html.CoerceTo.element parent >>?= fun parent ->
                parent##querySelector (Js.string "form") >>?=
                Dom_html.CoerceTo.form in
              Js.Opt.iter form (fun form ->
                let f = Form.get_form_contents form in
                let name = List.assoc "name" f |> String.trim in
                if name <> "" then (
                  let adder =
                    match node_type with
                    | `Action -> M.add_action
                    | `Project -> M.add_project
                    | `Area -> M.add_area in
                  Lwt_js_events.async (fun () -> adder m ~parent:item.M.details_uuid ~name ~description:"")
                );
                set_editing None;
              );
              true in
            React.S.const [
              form ~a:[a_onsubmit do_add] [
                input ~a:[a_name "name"; a_placeholder "Name"] ()];
                input ~a:[a_input_type `Submit; a_value "Add"; a_onclick do_add] ();
                a ~a:[a_onclick (fun _ev -> set_editing None; true)] [pcdata " (cancel)"];
              ]
      )
    in
    let rlist = rlist_of ~init:(React.S.value widgets) widgets in
    ul [
      R.Html5.li ~a:[a_class ["add"]] rlist
    ]

  let make_details_panel m ~show_node ~remove ~uuid item =
    let closed, set_closed = React.S.create false in
    let close () =
      let open Lwt in
      set_closed true;                          (* Start fade-out animation *)
      Lwt.async (fun () -> Lwt_js.sleep 0.5 >|= remove)  (* Actually remove *)
    in
    let cl =
      closed >>~= (fun closed ->
        current_highlight |> React.S.map (fun highlight ->
          "ck-details" :: List.concat [
            if highlight = Some uuid then ["ck-highlight"] else [];
            if closed then ["closed"] else [];
          ]
        )
      ) in
    let title_cl =
      item.M.details_type >|~= (fun node_type -> ["ck-title"; class_of_node_type node_type]) in
    let children = item.M.details_children
      |> Delay.make ~delay:1.0
      |> ReactiveData.RList.map (make_node_view m ~show_node) in
    div ~a:[R.Html5.a_class cl] [
      a ~a:[a_onclick (fun _ -> close (); true); a_class ["close"]] [entity "#215"];
      h4 ~a:[R.Html5.a_class title_cl] [R.Html5.pcdata item.M.details_name];
      R.Html5.ul children;
      make_child_adder m item;
      div ~a:[a_class ["description"]] [
        p [R.Html5.pcdata item.M.details_description];
      ]
    ]

  let make_details_area m =
    let details_pane, details_handle = ReactiveData.RList.make [] in
    let rec show_node uuid =
      let remove () =
        let current_items = ReactiveData.RList.value details_pane in
        match index_of uuid current_items with
        | None -> ()
        | Some i-> ReactiveData.RList.remove i details_handle in
      let current_items = ReactiveData.RList.value details_pane in
      let existing =
        try
          Some (List.find (fun (id, _) -> id = uuid) current_items)
        with Not_found -> None in
      match existing with
      | None ->
          let details = M.details m uuid in
          ReactiveData.RList.insert (uuid, make_details_panel m ~show_node ~remove ~uuid details) (List.length current_items) details_handle;
      | Some _ ->
          let open Lwt in
          set_highlight (Some uuid);
          Lwt.async (fun () ->
            Lwt_js.sleep 1.0 >|= fun () ->
            if React.S.value current_highlight = Some uuid then set_highlight None
          )
      in
    (ReactiveData.RList.map snd details_pane, show_node)

  let make_top m =
    let current_mode, set_current_mode = React.S.create `Process in
    let details_area, show_node = make_details_area m in
    [
      make_mode_switcher current_mode set_current_mode;
      div ~a:[a_class ["row"]] [
        div ~a:[a_class ["medium-6"; "columns"; "ck-tree"]] [
          make_tree ~show_node current_mode m;
        ];
        R.Html5.div ~a:[a_class ["medium-6"; "columns"]] (
          details_area;
        );
      ]
    ]
end
