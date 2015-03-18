(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Tyxml_js
open Html5
open Ck_utils

let (>|=) = Lwt.(>|=)

module Gui_tree_data = struct
  (* If the gui_data for a widget is None then it has just appeared.
   * We set the value to the newly-created element.
   *
   * If an item appears with a value already set then this is a move.
   * We read the old data to get the old height (needed for the animation),
   * and then update it to point at the new element.
   *)
  type t = Dom_html.element Js.t
end

let async ~name (fn:unit -> unit Lwt.t) =
  Lwt_js_events.async (fun () ->
    Lwt.catch fn (fun ex ->
      Printf.printf "Async error in '%s'" name;
      Lwt.fail ex
    )
  )

let fmt_date date =
  let open Unix in
  let tm = localtime date in
  Printf.sprintf "%04d-%02d-%02d (%s)"
    (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday (string_of_day tm.tm_wday)

let fmt_timestamp date =
  let open Unix in
  let tm = localtime date in
  Printf.sprintf "%04d-%02d-%02d %02d:%02d (%s)"
    (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min (string_of_day tm.tm_wday)

let close_current_model = ref None

let rec inside elem child =
  if elem == child then true
  else (
    Js.Opt.case (child##parentNode)
      (fun () -> false)
      (fun p -> inside elem p)
  )

let ignore_listener : Dom_html.event_listener_id -> unit = ignore

let close_modal () =
  match !close_current_model with
  | None -> ()
  | Some (_, close) ->
      close_current_model := None;
      close ()

(* Listen to global clicks and keypresses so we can close modals on click/escape *)
let keycode_escape = 27
let () =
  let click (ev:#Dom_html.mouseEvent Js.t) =
    match !close_current_model with
    | None -> Js._true
    | Some (elem, _close) ->
        Js.Opt.case (ev##target)
          (fun () -> Js._true)
          (fun target ->
            if (target :> Dom.node Js.t) |> inside elem then (
              (* Click inside modal - pass it on *)
              Js._true
            ) else (
              (* Click outside modal; close the modal *)
              close_modal ();
              Dom_html.stopPropagation ev;
              Js._false
            )
          ) in
  let keyup ev =
    match !close_current_model with
    | Some _ when ev##keyCode = keycode_escape ->
        close_modal ();
        Dom_html.stopPropagation ev;
        Js._false
    | _ -> Js._true in
  Dom_html.addEventListener Dom_html.document Dom_html.Event.click (Dom.handler click) Js._true |> ignore_listener;
  Dom_html.addEventListener Dom_html.document Dom_html.Event.keypress (Dom.handler keyup) Js._true |> ignore_listener

let pos_from_root (elem : #Dom_html.element Js.t) =
  let rec aux x y elem =
    let x = x + elem##offsetLeft in
    let y = y + elem##offsetTop in
    Js.Opt.case (elem##offsetParent)
      (fun () -> (x, y))
      (fun parent -> aux x y parent) in
  aux 0 0 elem

let show_modal, modal_div =
  close_modal ();
  let dropdown, set_dropdown = ReactiveData.RList.make [] in
  let dropdown_style, set_dropdown_style = React.S.create "" in
  let modal_div =
    R.Html5.div ~a:[a_class ["f-dropdown"; "ck-modal"]; R.Html5.a_style dropdown_style] dropdown in
  let close () =
    ReactiveData.RList.set set_dropdown [];
    set_dropdown_style "";
    close_current_model := None in
  let show ~parent content =
    let left, bottom =
      Js.Opt.case parent
        (fun () -> 10, 10)
        (fun parent ->
            let left, top = pos_from_root parent in
            let height = parent##offsetHeight in
            (left, top + height)) in
    ReactiveData.RList.set set_dropdown content;
    set_dropdown_style (Printf.sprintf "position: absolute; left: %dpx; top: %dpx;" left bottom);
    close_current_model := Some (Tyxml_js.To_dom.of_node modal_div, close) in
  (show, modal_div)

let current_error, set_current_error = React.S.create None

let make_error_box error =
  error
  |> React.S.map (function
    | None -> pcdata ""
    | Some err ->
        div ~a:[a_class ["alert-box"; "alert"]] [
          p [pcdata err]; p [pcdata "Refresh this page to continue."];
        ]
  )
  |> ReactiveData.RList.singleton_s

let () =
  let old_hook = !Lwt.async_exception_hook in
  Lwt.async_exception_hook := (fun ex ->
    old_hook ex;
    let msg = Printexc.to_string ex in
    set_current_error (Some msg)
  )

(* Get the index of an item in an assoc list. *)
let index_of key items =
  let rec aux i = function
    | [] -> None
    | (k, _v) :: _ when k = key -> Some i
    | _ :: xs -> aux (i + 1) xs in
  aux 0 items

let auto_focus input =
  async ~name:"focus" (fun () ->
    let elem = Tyxml_js.To_dom.of_input input in
    elem##select ();
    Lwt.return ()
  )

let (>>?=) = Js.Opt.bind

module Make (M : Ck_model_s.MODEL with type gui_data = Gui_tree_data.t) = struct
  module W = M.Widget

  let with_done cls = function
    | `Action _ | `Project _ as node when M.Item.is_done node -> "ck-done" :: cls
    | _ -> cls

  let class_of_node_type = function
    | `Area _ -> "ck-area"
    | `Project _ -> "ck-project"
    | `Action _ -> "ck-action"
    | `Contact _ -> "ck-contact"
    | `Deleted -> "ck-deleted"

  let class_of_time_and_type ctime item =
    let ty = with_done ["ck-item"; class_of_node_type item] item in
    let lifetime = Unix.gettimeofday () -. ctime in
    if lifetime >= 0.0 && lifetime <  1.0 then "new" :: ty
    else ty

  let toggle_of_astate = function
    | `Done | `Next | `Waiting | `Future as s -> s
    | `Waiting_for_contact _ | `Waiting_until _ -> `Waiting

  let waiting_candidates m item =
    M.candidate_contacts_for m item |> List.map (fun candidate ->
      let clicked _ev =
        close_modal ();
        async ~name:"set waiting" (fun () -> M.choose_candidate candidate);
        true in
      li [
        a ~a:[a_onclick clicked] [
          pcdata (M.candidate_label candidate)
        ]
      ]
    )

  let toggle_label ~set_details ~current ~due details =
    let l =
      match details with
      | `Done -> "✓"
      | `Next -> "n"
      | `Waiting -> "w"
      | `Future -> "f"
      | `Active -> "a"
      | `SomedayMaybe -> "sm" in
    let cl =
      if due && l = "w" then "ck-active-alert"
      else if current = details then "ck-active-" ^ l else "ck-inactive" in
    let changed ev =
      set_details ev details;
      true in
    a ~a:[a_class [cl]; a_onclick changed] [pcdata l]

  let make_toggles ~m ~set_details ~item ?(due=false) current options =
    let starred = M.Item.starred item in
    let state_toggles = options |> List.map (toggle_label ~set_details ~current ~due) in
    let cl = if starred then "star-active" else "star-inactive" in
    let set_star _ev =
      async ~name:"set_starred" (fun () -> M.set_starred m item (not starred));
      true in
    let star = a ~a:[a_class [cl]; a_onclick set_star] [pcdata "★"] in
    state_toggles @ [star]

  let due action =
    match M.Item.action_state action with
    | `Waiting_until time -> Some time
    | _ -> None

  let wait_until_date m action =
    let on_select date =
      async ~name:"waiting until" (fun () -> M.set_action_state m action (`Waiting_until date));
      close_modal () in
    Pikaday.make ?initial:(due action) ~on_select ()

  let toggles_for_type m = function
    | `Action action as item ->
        let current = M.Item.action_state action |> toggle_of_astate in
        let set_details ev n =
          if n = `Waiting then (
            let content = table [
              tr [
                td [wait_until_date m action];
                td ~a:[a_class ["ck-or"]] [pcdata "or"];
                td ~a:[a_class ["ck-waiting-menu"]] [ul (waiting_candidates m item)]
              ]
            ] in
            show_modal ~parent:(ev##target) [content];
          ) else if current <> n then (
            async ~name:"set_action_state" (fun () -> M.set_action_state m action n)
          ) in
        let due = M.Item.is_due action in
        make_toggles ~m ~set_details ~item ~due current [`Done; `Next; `Waiting; `Future]
    | `Project project as item ->
        let set_details _ev n =
          async ~name:"set_project_state" (fun () -> M.set_project_state m project n) in
        make_toggles ~m ~set_details ~item (M.Item.project_state project) [`Done; `Active; `SomedayMaybe]
    | `Area _ | `Contact _ -> []

  let make_state_toggles m item =
    let toggles = item >|~= (function
      | Some item -> toggles_for_type m item
      | None -> []
    ) in
    rlist_of ~init:(React.S.value toggles) toggles

  (* Fade item in and out based on state. *)
  let animated widget child_nodes =
    let li_state =
      W.state widget >|~= fun state ->
        let gui_data = W.gui_data widget in
        let old_item = !gui_data in   (* The original that is being moved, if any *)
        match state, old_item with
        | `Removed _time, _ -> `Fade_out
        | (`New | `Init), Some old_item -> `Fade_in_from old_item
        | `Current, _ -> `No_animation
        | `Init, None -> `No_animation
        | `New, None -> `Fade_in
        in
    let li_cl = li_state |> React.S.map (function
        | `Fade_in -> ["new"]
        | `Fade_out -> ["removed"]
        | `Fade_in_from _ -> ["moved"]
        | `No_animation -> []
    ) in
    let li_elem = li ~a:[R.Html5.a_class li_cl] child_nodes in
    let cancel = ref ignore in
    let animate =
      li_state >|~= fun animation ->
        !cancel ();
        cancel := ignore;
        let gui_data = W.gui_data widget in
        (* Update to point at the new element *)
        gui_data := Some (Tyxml_js.To_dom.of_li li_elem);
        match animation with
        | `Fade_out ->
            let elem = Tyxml_js.To_dom.of_element li_elem in
            cancel := Ck_animate.fade_out elem;
        | `Fade_in_from old_item ->
            let full_height = old_item##offsetHeight in
            cancel := Ck_animate.fade_in_move ~full_height li_elem
        | `Fade_in -> ()    (* Handled by the CSS alone *)
        | `No_animation -> () in
    React.S.retain li_state (fun () -> ignore animate) |> ignore;
    li_elem

  let show_add_modal m ~show_node ~button item =
    let name_input = input ~a:[a_name "name"; a_placeholder "Name"; a_size 25] () in
    auto_focus name_input;
    let submit_clicked _ev =
      let input_elem = Tyxml_js.To_dom.of_input name_input in
      let name = input_elem##value |> Js.to_string |> String.trim in
      if name <> "" then (
        async ~name:"add child" (fun () ->
          M.add_child m item name >|= function
          | None -> ()
          | Some node -> show_node (node :> M.Item.generic)
        );
      );
      close_modal ();
      false in
    let content =
      form ~a:[a_onsubmit submit_clicked; a_action "#"] [
        name_input;
        input ~a:[a_input_type `Submit; a_value "Add"] ();
      ] in
    show_modal ~parent:button [content]

  let report_error ~parent = function
    | `Ok () -> ()
    | `Error msg ->
        let close _ev = close_modal (); false in
        let content =
          div ~a:[a_class ["alert-box"]; a_onclick close] [
            pcdata msg;
          ] in
        show_modal ~parent [content]

  let render_item m ~show_node (item : [< M.Item.generic]) =
    let clicked _ev = show_node (item :> M.Item.generic); true in
    let delete ev = async ~name:"delete" (fun () -> M.delete m item >|= report_error ~parent:ev##target); true in
    let item_cl = class_of_time_and_type (M.Item.ctime item) item in
    span ~a:[a_class item_cl] [
      span ~a:[a_class ["ck-toggles"]] (toggles_for_type m item);
      span ~a:[a_class ["allow-strikethrough"]] [   (* CSS hack to allow strikethrough and underline together *)
        a ~a:[a_class ["ck-title"]; a_href "#"; a_onclick clicked] [pcdata (M.Item.name item)];
      ];
      begin match item with
      | `Contact _ -> pcdata ""
      | `Action _ -> a ~a:[a_class ["delete"]; a_onclick delete] [entity "cross"]
      | `Area _ | `Project _ as item ->
          let add_child ev =
            show_add_modal m ~show_node ~button:(ev##target) item;
            true in
          a ~a:[a_class ["ck-add-child"]; a_onclick add_child] [pcdata "+"]
      end;
    ]

  let group_label s =
    span ~a:[a_class ["ck-group-label"]] [pcdata s]

  (* A <li>[toggles] name x [children]</li> element *)
  let rec make_tree_node_view m ~show_node widget : _ Html5.elt =
    let item = W.item widget in
    let children = W.children widget
      |> ReactiveData.RList.map (make_tree_node_view m ~show_node) in
    let item_html =
      match item with
      | `Item item ->
          ReactiveData.RList.singleton_s item
          |> ReactiveData.RList.map (render_item m ~show_node)
          |> R.Html5.span
      | `Group label -> group_label label in
    animated widget [
      item_html;
      R.Html5.ul children;
    ]

  let make_work_view m ~show_node top =
    let make_work_actions group =
      let item_html =
        match W.item group with
        | `Item item ->
            let show_group _ev =
              React.S.value item |> show_node;
              true in
            let name = item >|~= M.Item.name in
            a ~a:[a_class ["ck-group"]; a_onclick show_group] [R.Html5.pcdata name];
        | `Group label -> group_label label in
      animated group [
        item_html;
        R.Html5.ul (
          ReactiveData.RList.map (make_tree_node_view m ~show_node) (W.children group)
        )
      ] in
    match ReactiveData.RList.value top with
    | [] | [_] | _::_::_::_ -> assert false
    | [groups; done_actions] ->
    let heading widget =
      match W.item widget with
      | `Item _ -> pcdata "ERROR: not a heading!"
      | `Group label -> h4 [pcdata label] in
    let next_children = W.children groups |> ReactiveData.RList.map make_work_actions in
    let done_children = W.children done_actions |> ReactiveData.RList.map (make_tree_node_view m ~show_node) in
    [
      div ~a:[a_class ["ck-next-actions"]] [
        heading groups;
        R.Html5.ul next_children;
      ];
      div ~a:[a_class ["ck-done-actions"]] [
        heading done_actions;
        R.Html5.ul done_children;
      ];
    ]

  let make_sync m =
    let items = M.log m
      |> ReactiveData.RList.map (fun slow_item ->
          let item_ref = ref None in
          let cancel = ref ignore in
          let cl =
            Slow_set.state slow_item >|~= fun state ->
              !cancel ();
              match state with
              | `New -> ["new"]
              | `Init | `Current -> []
              | `Removed _ ->
                  begin match !item_ref with
                  | None -> ()
                  | Some item ->
                      let elem = Tyxml_js.To_dom.of_li item in
                      cancel := Ck_animate.fade_out elem end;
                  [] in
          let log_entry = Slow_set.data slow_item in
          let view _ev =
            async ~name:"fix_head" (fun () -> M.fix_head m (Some log_entry));
            false in
          let open Git_storage_s.Log_entry in
          let summary =
            match log_entry.msg with
            | [] -> "(no log message)"
            | x::_ -> x in
          let date = fmt_timestamp log_entry.date in
          let item =
            li ~a:[R.Html5.a_class cl] [
              a ~a:[a_onclick view] [pcdata date];
              p [pcdata summary];
            ] in
          item_ref := Some item;
          item
      ) in
    div [
      R.Html5.ol ~a:[a_class ["ck-history"]] items;
    ]

  let show_add_contact m ~show_node ~parent =
    let name_input = input ~a:[a_name "name"; a_placeholder "Name"; a_size 25] () in
    auto_focus name_input;
    let submit_clicked _ev =
      let input_elem = Tyxml_js.To_dom.of_input name_input in
      let name = input_elem##value |> Js.to_string |> String.trim in
      if name <> "" then (
        async ~name:"add contact" (fun () ->
          M.add_contact m ~name >|= function
          | None -> ()
          | Some node -> show_node (node :> M.Item.generic)
        );
      );
      close_modal ();
      false in
    let content =
      form ~a:[a_onsubmit submit_clicked; a_action "#"] [
        name_input;
        input ~a:[a_input_type `Submit; a_value "Add"] ();
      ] in
    show_modal ~parent [content]

  let make_contact_view m ~show_node tree =
    let add_clicked ev =
      show_add_contact m ~show_node ~parent:ev##target;
      false in
    [
      h4 [pcdata "Contacts"; a ~a:[a_class ["ck-add-child"]; a_onclick add_clicked] [pcdata "+"]];
      R.Html5.ul (
        ReactiveData.RList.map (make_tree_node_view m ~show_node) tree
      )
    ]

  let make_schedule_view m ~show_node tree =
    let add_clicked ev =
      let name_input = input ~a:[a_name "name"; a_placeholder "Name"; a_size 20] () in
      auto_focus name_input;
      let on_select date =
        let input_elem = Tyxml_js.To_dom.of_input name_input in
        let name = input_elem##value |> Js.to_string |> String.trim in
        if name <> "" then (
          async ~name:"add to schedule" (fun () ->
            M.add_action m ~state:(`Waiting_until date) ?parent:None ~name ~description:"" >|= function
            | None -> print_endline "Added item no longer exists!"
            | Some item -> show_node (item :> M.Item.generic)
          );
          close_modal ()
        ) in
      let content = div ~a:[a_class ["ck-add-scheduled"]] [
        name_input;
        Pikaday.make ~on_select ()
      ] in
      show_modal ~parent:(ev##target) [content];
      false in
    [
      h4 [pcdata "Schedule"; a ~a:[a_class ["ck-add-child"]; a_onclick add_clicked] [pcdata "+"]];
      R.Html5.ul (
        ReactiveData.RList.map (make_tree_node_view m ~show_node) tree
      )
    ]

  let make_tree ~show_node m = function
    | `Process tree | `Review tree ->
        [R.Html5.ul (
          ReactiveData.RList.map (make_tree_node_view m ~show_node) tree
        )]
    | `Contact tree -> make_contact_view m ~show_node tree
    | `Work work_tree -> make_work_view m ~show_node work_tree
    | `Schedule tree -> make_schedule_view m ~show_node tree

  let mode_of = function
    | `Process _ -> `Process
    | `Work _ -> `Work
    | `Review _ -> `Review
    | `Contact _ -> `Contact
    | `Schedule _ -> `Schedule

  let make_mode_switcher m current_tree =
    let item ?alert name mode =
      let cl = current_tree |> React.S.map (fun t ->
        if (mode_of t) = mode then ["active"] else []
      ) in
      let clicked _ev = M.set_mode m mode; false in
      let attrs = [a_onclick clicked] in
      let attrs =
        match alert with
        | None -> attrs
        | Some s ->
            let cl = R.Html5.a_class (s >|~= function
              | false -> [""]
              | true -> ["alert"]
            ) in
            cl :: attrs in
      let button = a ~a:attrs [pcdata name] in
      dd ~a:[R.Html5.a_class cl] [button] in

    dl ~a:[a_class ["sub-nav"]] [
      item "Process" `Process;
      item "Work" `Work ~alert:(M.alert m);
      item "Contact" `Contact;
      item "Schedule" `Schedule;
      item "Review" `Review;
    ]

  let assume_changed _ _ = false

  let add_form ~close ~show_node adder =
    let do_add ev =
      let form = ev##target >>?= Dom_html.CoerceTo.form in
      Js.Opt.iter form (fun form ->
        let f = Form.get_form_contents form in
        let name = List.assoc "name" f |> String.trim in
        if name <> "" then (
          async ~name:"add" (fun () ->
            adder ~name ~description:"" >|= function
            | None -> print_endline "Added item no longer exists!"
            | Some item -> show_node (item :> M.Item.generic)
          )
        );
        close ()
      );
      false in
    let keydown (ev:Dom_html.keyboardEvent Js.t) =
      if ev##keyCode = keycode_escape then (
        close();
        false
      ) else true in
    let name_input = input ~a:[a_name "name"; a_placeholder "Name"] () in
    auto_focus name_input;
    form ~a:[a_onsubmit do_add; a_onkeydown keydown] [
      name_input;
    ]

  let make_child_adder m ~show_node item =
    let editing, set_editing = React.S.create ~eq:assume_changed None in
    let add_button adder label =
      let start_editing (_:#Dom_html.event Js.t) =
        set_editing (Some adder);
        true in
      li [a ~a:[a_onclick start_editing] [pcdata label]] in
    let widgets =
      editing >>~= (function
        (* When we're not editing, display the add buttons. *)
        | None ->
            item >|~= (function
              | None -> pcdata ""
              | Some item ->
                  match item with
                  | `Action _ | `Contact _ -> pcdata ""
                  | `Project _ as item -> ul ~a:[a_class ["add"]] [
                      add_button (M.add_project m ~parent:item) "+sub-project";
                      add_button (M.add_action m ~state:`Next ~parent:item) "+action";
                    ]
                  | `Area _ as item -> ul ~a:[a_class ["add"]] [
                      add_button (M.add_area m ~parent:item) "+sub-area";
                      add_button (M.add_project m ~parent:item) "+project";
                      add_button (M.add_action m ~state:`Next ~parent:item) "+action";
                    ]
            )
        (* When we are editing, display the form. *)
        | Some adder ->
            let close () = set_editing None in
            React.S.const (add_form ~close ~show_node adder)
      )
    in
    let rlist = ReactiveData.RList.singleton_s widgets in
    R.Html5.div ~a:[a_class ["ck-adders"]] rlist

  let make_editable_title m item =
    let name = item >|~= (function
      | Some item -> M.Item.name item
      | None -> "(deleted)"
    ) in
    let editing, set_editing = React.S.create None in
    let widgets =
      editing >|~= (function
        | None ->
            let edit _ev =
              begin match React.S.value item with
              | None -> ()
              | Some item -> set_editing (Some item) end;
              true in
            [
              span ~a:[a_class ["allow-strikethrough"]] [   (* CSS hack to allow strikethrough and underline together *)
                a ~a:[a_class ["ck-title"]; a_onclick edit] [R.Html5.pcdata name];
              ]
            ]
        | Some item ->
            let submit ev =
              let form = ev##target >>?= Dom_html.CoerceTo.form in
              Js.Opt.iter form (fun form ->
                let f = Form.get_form_contents form in
                let name = List.assoc "name" f |> String.trim in
                if name <> "" then (
                  async ~name:"set_name" (fun () -> M.set_name m item name)
                )
              );
              set_editing None;
              false in
            let old_name = React.S.value name in
            let name_input = input ~a:[a_name "name"; a_placeholder "Name"; a_size 25; a_value old_name] () in
            auto_focus name_input;
            [
              form ~a:[a_class ["rename"]; a_onsubmit submit] [
                name_input;
              ]
            ]
      ) in
    rlist_of ~init:(React.S.value widgets) widgets

  let parent_candidates m item =
    M.candidate_parents_for m item |> List.map (fun candidate ->
      let clicked _ev =
        close_modal ();
        async ~name:"set parent" (fun () -> M.choose_candidate candidate);
        true in
      li [
        a ~a:[a_onclick clicked] [
          pcdata (M.candidate_label candidate)
        ]
      ]
    )

  let label s = span ~a:[a_class ["ck-label"]] [pcdata s]

  let show_type_modal m ~button item =
    let make label fn item =
      let clicked _ev =
        close_modal ();
        async ~name:label (fun () -> fn m item >|= report_error ~parent:button);
        false in
      li [a ~a:[a_onclick clicked] [pcdata label]] in
    let content = ul (
      match item with
      | `Action _ as item -> [make "Convert to project" M.convert_to_project item]
      | `Project p -> [make "Convert to action" M.convert_to_action p;
                       make "Convert to area" M.convert_to_area p]
      | `Area _ as item -> [make "Convert to project" M.convert_to_project item]
    ) in
    show_modal ~parent:button [content]

  let make_parent_details m ~show_node details =
    let title =
      details.M.details_parent >|~= function
        | None -> pcdata "(no parent)"
        | Some parent ->
            let parent = (parent :> M.Item.generic) in
            let cl = ["ck-item"; class_of_node_type parent] in
            let clicked _ev = show_node parent; true in
            span ~a:[a_class cl] [
              a ~a:[a_class ["ck-title"]; a_onclick clicked] [pcdata (M.Item.name parent)]
            ] in
    let title_elem = R.Html5.span (ReactiveData.RList.singleton_s title) in
    let descr =
      details.M.details_item >|~= function
        | None -> [pcdata "(deleted)"]
        | Some (`Contact _) -> [label "A contact"]
        | Some (`Area _ | `Project _ | `Action _ as item) ->
            let change_type label =
              let on_click ev =
                show_type_modal m ~button:(ev##target) item; false in
              a ~a:[a_onclick on_click] [pcdata label] in
            let change_clicked ev =
              let content = ul (parent_candidates m item) in
              show_modal ~parent:(ev##target) [content];
              false in
            let change_button = a ~a:[a_onclick change_clicked] [pcdata " (change)"] in
            match item with
            | `Action _ -> [label "An "; change_type "action"; label " in "; title_elem; change_button]
            | `Project _ -> [label "A "; change_type "project"; label " in "; title_elem; change_button]
            | `Area _ -> [label "An "; change_type "area"; label " in "; title_elem; change_button] in
    rlist_of ~init:(React.S.value descr) descr

  let make_editable_description m item =
    let editing, set_editing = React.S.create None in
    let elements =
      editing >>~= (function
        | None ->
            item >|~= (function
              | None -> []
              | Some item ->
                  let clicked _ev =
                    set_editing (Some item);
                    false in
                  let raw_html = M.Item.description item |> Omd.of_string |> Omd.to_html in
                  let description = div [] in
                  let () =
                    let elem = Tyxml_js.To_dom.of_div description in
                    elem##innerHTML <- Js.string raw_html in
                  [
                    description;
                    a ~a:[a_class ["edit"]; a_onclick clicked] [pcdata "(edit)"];
                  ]
            );
        | Some item ->
            let cancel _ev = set_editing None; false in
            let submit_ref = ref (fun _ -> true) in
            let keydown (ev:Dom_html.keyboardEvent Js.t) =
              if ev##keyCode = 13 && Js.to_bool ev##ctrlKey then !submit_ref ev
              else true in
            let value = textarea ~a:[a_rows 5; a_onkeydown keydown]
              (pcdata (M.Item.description item)) in
            async ~name:"focus" (fun () ->
              let elem = Tyxml_js.To_dom.of_textarea value in
              elem##focus ();
              Lwt.return ()
            );
            let submit _ev =
              let elem = Tyxml_js.To_dom.of_textarea value in
              let v = Js.to_string (elem##value) |> String.trim in
              async ~name:"set_description" (fun () -> M.set_description m item v);
              set_editing None;
              false in
            submit_ref := submit;
            React.S.const [
              form ~a:[a_onsubmit submit] [
                value;
                div ~a:[a_class ["actions"]] [
                  a ~a:[a_onclick cancel] [pcdata "(cancel) "];
                  input ~a:[a_input_type `Submit; a_value "OK"] ();
                ]
              ]
            ]
      ) in
    let cl = editing >|~= (function
      | None -> ["description"]
      | Some _ -> ["description-form"]
    ) in
    R.Html5.div ~a:[R.Html5.a_class cl] (rlist_of elements)

  let edit_date ~ev m action =
    let on_select date =
      async ~name:"waiting until" (fun () -> M.set_action_state m action (`Waiting_until date));
      close_modal () in
    let content = Pikaday.make ?initial:(due action) ~on_select () in
    show_modal ~parent:(ev##target) [content]

  let make_details_panel m ~set_closed ~show_node details =
    let item = details.M.details_item in
    let title_cl =
      item >|~= (function
        | None ->
            set_closed true;
            ["ck-heading"]
        | Some item ->
            with_done ["ck-heading"; class_of_node_type item] item
      ) in
    let children = details.M.details_children
      |> ReactiveData.RList.map (make_tree_node_view m ~show_node) in
    let initial_item = React.S.value details.M.details_item in
    let delete_clicked ev =
      begin match React.S.value item with
      | None -> ()
      | Some item -> async ~name:"delete" (fun () -> M.delete m item >|= report_error ~parent:(ev##target)) end;
      false in
    let ctime =
      match initial_item with
      | None -> "-"
      | Some item -> M.Item.ctime item |> fmt_timestamp in
    let description = make_editable_description m item in
    let title = 
      div ~a:[R.Html5.a_class title_cl] [
        R.Html5.span ~a:[a_class ["ck-toggles"]] (make_state_toggles m item);
        R.Html5.div ~a:[a_class ["inline"]] (make_editable_title m item);
      ] in
    let waiting_for =
      item >|~= (function
        | Some (`Action action) ->
            begin match M.Item.action_state action with
            | `Waiting_until time ->
                let clicked ev = edit_date ~ev m action; false in
                [label "Waiting until "; a ~a:[a_onclick clicked] [pcdata (fmt_date time)]]
            | _ -> []
            end
        | _ -> []
      ) |> rlist_of in
    let contents =
      div [
        div [
          R.Html5.span (make_parent_details m ~show_node details);
        ];
        div [
          label ("Created " ^ ctime);
          a ~a:[a_onclick delete_clicked; a_class ["ck-delete"]] [pcdata " (delete)"];
        ];
        R.Html5.div waiting_for;
        R.Html5.ul ~a:[a_class ["ck-groups"]] children;
        make_child_adder m ~show_node item;
        description;
      ] in
    (title, contents)

  let make_toplevel_adders m ~show_node =
    let editing, set_editing = React.S.create ~eq:assume_changed None in
    let make adder label =
      let clicked _ev =
        set_editing (Some adder);
        true in
      li [a ~a:[a_onclick clicked] [pcdata label]] in
    let widget =
      editing >|~= function
      | None ->
          ul ~a:[a_class ["ck-adders"]] [
            make (M.add_area m ?parent:None) "+area";
            make (M.add_project m ?parent:None) "+project";
            make (M.add_action m ~state:`Next ?parent:None) "+action";
          ]
      | Some adder ->
          let close () = set_editing None in
          add_form ~close ~show_node adder in
    ReactiveData.RList.singleton_s widget

  let history_uuid = Ck_id.of_string "aeeb4ba1-ae68-43ff-b23e-1f66e8b950a3"

  let make_details_area m =
    let details_pane, details_handle = ReactiveData.RList.make [] in
    let remove uuid =
      let current_items = ReactiveData.RList.value details_pane in
      match index_of uuid current_items with
      | None -> ()
      | Some i-> ReactiveData.RList.remove i details_handle in
    let show_or_create uuid make =
      let current_items = ReactiveData.RList.value details_pane in
      let existing =
        try
          Some (List.find (fun (id, _) -> id = uuid) current_items)
        with Not_found -> None in
      match existing with
      | None ->
          let closed, set_closed = React.S.create false in
          let panel = make closed set_closed in
          ReactiveData.RList.insert (uuid, panel) 0 details_handle;
      | Some _ ->
          Ck_panel.highlight uuid
      in
    let rec show_node (item : [< M.Item.generic]) =
      let uuid = M.Item.uuid item in
      show_or_create uuid (fun closed set_closed ->
        let details = M.details m item in
        let on_destroy () =
          details.M.details_stop ();
          remove uuid in
        let title, contents = make_details_panel m ~set_closed ~show_node details in
        Ck_panel.make ~on_destroy ~closed ~set_closed ~title ~contents ~id:uuid
      ) in
    let show_history () =
      show_or_create history_uuid (fun closed set_closed ->
        let title = b [pcdata "History"] in
        let contents = make_sync m in
        Ck_panel.make ~on_destroy:(fun () -> remove history_uuid) ~closed ~set_closed ~title ~contents ~id:history_uuid
      ) in
    let close_all () =
      ReactiveData.RList.value details_pane
      |> List.iter (fun (_id, panel) -> Ck_panel.close panel) in
    (
      ReactiveData.RList.map (fun (_, panel) -> Ck_panel.element panel) details_pane,
      show_node,
      show_history,
      close_all
    )

  let time_travel_warning m =
    let showing = ref false in
    let return_to_present _ev =
      async ~name:"return_to_present" (fun () -> M.fix_head m None);
      false in
    M.fixed_head m >|~= (function
      | None -> showing := false; []
      | Some log_entry ->
          let cl = ["alert-box"; "radius"; "ck-time-travel-warning"] in
          let cl = if !showing then cl else "new" :: cl in
          showing := true;
          [
            div ~a:[a_class cl] [
              pcdata (
                Printf.sprintf "Time-travel active: you are viewing the state of CueKeeper as at %s."
                  (fmt_timestamp log_entry.Git_storage_s.Log_entry.date)
              );
              a ~a:[a_class ["close"]; a_onclick return_to_present] [pcdata "×"]
            ]
          ]
    )
    |> rlist_of

  let make_top m =
    let current_tree = M.tree m in
    let details_area, show_node, show_history, close_all = make_details_area m in
    let left_panel =
      let live = current_tree >|~= make_tree ~show_node m in
      rlist_of ~init:(React.S.value live) live in
    [
      modal_div;
      div ~a:[a_class ["row"]] [
        div ~a:[a_class ["medium-8"; "columns"]] [
          make_mode_switcher m current_tree;
        ];
        R.Html5.div ~a:[a_class ["medium-4"; "columns"; "ck-adders"]] (
          make_toplevel_adders m ~show_node
        );
      ];
      div ~a:[a_class ["row"]] [
        div ~a:[a_class ["small-12"; "columns"; "ck-actions"]] [
          a ~a:[a_onclick (fun _ -> show_history (); false)] [pcdata "Show history"];
          a ~a:[a_onclick (fun _ -> close_all (); false)] [pcdata "Close all"];
        ]
      ];
      div ~a:[a_class ["row"]] [
        R.Html5.div ~a:[a_class ["medium-12"; "columns"]] (
          time_travel_warning m;
        );
        R.Html5.div ~a:[a_class ["medium-12"; "columns"]] (
          make_error_box current_error;
        )
      ];
      div ~a:[a_class ["row"]] [
        R.Html5.div ~a:[a_class ["medium-6"; "columns"; "ck-tree"]] (
          left_panel;
        );
        R.Html5.div ~a:[a_class ["medium-6"; "columns"]] (
          details_area;
        );
      ];
    ]
end
