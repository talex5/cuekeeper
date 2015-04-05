(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Tyxml_js
open Html5
open Ck_utils
open Ck_js_utils

open Lwt.Infix

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

let show_modal, modal_div =
  let dropdown, set_dropdown = ReactiveData.RList.make [] in
  let dropdown_style, set_dropdown_style = React.S.create "" in
  let modal_div =
    R.Html5.div ~a:[a_class ["f-dropdown"; "ck-modal"]; R.Html5.a_style dropdown_style] dropdown in
  let close () =
    ReactiveData.RList.set set_dropdown [];
    set_dropdown_style "" in
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
    Ck_modal.show ~close (Tyxml_js.To_dom.of_element modal_div) in
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

let ck_label s = span ~a:[a_class ["ck-label"]] [pcdata s]

let (>>?=) = Js.Opt.bind

module Make (M : Ck_model_s.MODEL with type gui_data = Gui_tree_data.t) = struct
  module W = M.Widget

  let opt_show ~show_node = function
    | None -> print_endline "Added item no longer exists!"
    | Some node -> show_node (node :> M.Item.generic)

  let with_done cls = function
    | `Action _ | `Project _ as node when M.Item.is_done node -> "ck-done" :: cls
    | _ -> cls

  let class_of_node_type = function
    | `Area _ -> "ck-area"
    | `Project _ -> "ck-project"
    | `Action _ -> "ck-action"
    | `Contact _ -> "ck-contact"
    | `Context _ -> "ck-context"
    | `Deleted -> "ck-deleted"

  let class_of_time_and_type ctime item =
    let ty = with_done ["ck-item"; class_of_node_type item] item in
    let lifetime = Unix.gettimeofday () -. ctime in
    if lifetime >= 0.0 && lifetime <  1.0 then "new" :: ty
    else ty

  let toggle_of_astate = function
    | `Done | `Next | `Waiting | `Future as s -> s
    | `Waiting_for_contact | `Waiting_until _ -> `Waiting

  let waiting_candidates m action =
    let wait_unspec _ev =
      async ~name:"waiting" (fun () -> M.set_action_state m action `Waiting);
      Ck_modal.close ();
      false in
    let waiting = li [a ~a:[a_onclick wait_unspec] [pcdata "Waiting (reason unspecified)"]] in
    match M.Item.contact_node action with
    | None -> [waiting]
    | Some contact ->
        let clicked _ev =
          async ~name:"waiting-for" (fun () -> M.set_action_state m action `Waiting_for_contact);
          Ck_modal.close ();
          false in
        let name = M.Item.name contact in
        let waiting_for = li [a ~a:[a_onclick clicked] [pcdata ("Waiting for " ^ name)]] in
        [waiting_for; waiting]

  let is_repeat_action = function
    | `Action _ as a -> M.Item.action_repeat a <> None
    | _ -> false

  let toggle_label ~set_details ~current ~due item details =
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
      else if l = "✓" && is_repeat_action item then "ck-toggle-hidden"
      else if current = details then "ck-active-" ^ l else "ck-inactive" in
    let changed ev =
      set_details ev details;
      true in
    a ~a:[a_class [cl]; a_onclick changed] [pcdata l]

  let make_toggles ~m ~set_details ~item ?(due=false) current options =
    let starred = M.Item.starred item in
    let state_toggles = options |> List.map (toggle_label ~set_details ~current ~due item) in
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
      Ck_modal.close () in
    let initial =
      match M.Item.action_repeat action with
      | Some r -> Some (Ck_time.next_repeat ~now:(Unix.gettimeofday () |> Ck_time.of_unix_time) r)
      | None -> due action in
    Pikaday.make ?initial ~on_select () |> fst

  let toggles_for_type m = function
    | `Action _ as item ->
        let current = M.Item.action_state item |> toggle_of_astate in
        let set_details ev n =
          if n = `Waiting then (
            let content = div [
              wait_until_date m item;
              div ~a:[a_class ["ck-or"]] [pcdata "or"];
              ul ~a:[a_class ["ck-waiting-menu"]] (waiting_candidates m item)
            ] in
            show_modal ~parent:(ev##target) [content];
          ) else if current <> n then (
            async ~name:"set_action_state" (fun () -> M.set_action_state m item n)
          ) in
        let due = M.Item.is_due item in
        make_toggles ~m ~set_details ~item ~due current [`Done; `Next; `Waiting; `Future]
    | `Project _ as item ->
        let set_details _ev n =
          async ~name:"set_project_state" (fun () -> M.set_project_state m item n) in
        make_toggles ~m ~set_details ~item (M.Item.project_state item) [`Done; `Active; `SomedayMaybe]
    | `Area _ | `Contact _ | `Context _ -> []

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

  (** Display a floating box for a name, over [button]. If a non-empty name is entered,
   * call [adder name] and display the resulting item. *)
  let show_add_modal ~show_node ~button adder =
    let name_input = input ~a:[a_name "name"; a_placeholder "Name"; a_size 25] () in
    auto_focus name_input;
    let submit_clicked _ev =
      let input_elem = Tyxml_js.To_dom.of_input name_input in
      let name = input_elem##value |> Js.to_string |> String.trim in
      if name <> "" then (
        async ~name:"add" (fun () -> adder name >|= opt_show ~show_node);
      );
      Ck_modal.close ();
      false in
    let content =
      form ~a:[a_onsubmit submit_clicked] [
        name_input;
        input ~a:[a_input_type `Submit; a_value "Add"] ();
      ] in
    show_modal ~parent:button [content]

  let report_error ~parent = function
    | `Ok () -> ()
    | `Error msg ->
        let close _ev = Ck_modal.close (); false in
        let content =
          div ~a:[a_class ["alert-box"]; a_onclick close] [
            pcdata msg;
          ] in
        show_modal ~parent [content]

  let with_adder m ?adder ~show_node item_span =
    match adder with
    | Some adder ->
        let add_clicked ev =
          show_add_modal ~show_node ~button:(ev##target) (M.apply_adder m adder);
          false in
        span [
          item_span;
          a ~a:[a_class ["ck-add-child"]; a_onclick add_clicked] [pcdata "+"]
        ]
    | _ -> span [item_span]

  let render_item m ?adder ~show_node (item : [< M.Item.generic]) =
    let clicked _ev = show_node (item :> M.Item.generic); false in
    let delete ev = async ~name:"delete" (fun () -> M.delete m item >|= report_error ~parent:ev##target); true in
    let item_cl = class_of_time_and_type (M.Item.ctime item) item in
    span ~a:[a_class item_cl] [
      span ~a:[a_class ["ck-toggles"]] (toggles_for_type m item);
      span ~a:[a_class ["allow-strikethrough"]] [   (* CSS hack to allow strikethrough and underline together *)
        a ~a:[a_class ["ck-title"]; a_onclick clicked] [pcdata (M.Item.name item)];
      ];
      begin match item with
      | `Contact _ -> pcdata ""
      | `Context _ -> pcdata ""
      | `Action _ -> a ~a:[a_class ["delete"]; a_onclick delete] [entity "cross"]
      | `Area _ | `Project _ as item ->
          let add_child ev =
            show_add_modal ~show_node ~button:(ev##target) (M.add_child m item);
            true in
          a ~a:[a_class ["ck-add-child"]; a_onclick add_child] [pcdata "+"]
      end;
    ]
    |> with_adder m ?adder ~show_node

  let render_group_item m ?adder ~show_node item =
    let clicked _ev = show_node (item :> M.Item.generic); false in
    a ~a:[a_onclick clicked; a_class ["ck-group-label"]] [pcdata (M.Item.name item)]
    |> with_adder m ?adder ~show_node

  let group_label m ?adder ~show_node s =
    span ~a:[a_class ["ck-group-label"]] [pcdata s]
    |> with_adder m ?adder ~show_node

  (* A <li>[toggles] name x [children]</li> element *)
  let rec make_tree_node_view m ?(always_full=false) ~show_node widget : _ Html5.elt =
    let item = W.item widget in
    let children = W.children widget
      |> ReactiveData.RList.map (make_tree_node_view m ~always_full ~show_node) in
    let item_html =
      match item with
      | `Item item ->
          ReactiveData.RList.singleton_s item
          |> ReactiveData.RList.map (fun item ->
            let adder = W.adder widget in
            if always_full || W.unique widget then render_item m ~show_node ?adder item
            else render_group_item m ~show_node ?adder item
          )
          |> R.Html5.span
      | `Group label -> group_label m ?adder:(W.adder widget) ~show_node label in
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
            let add_clicked ev =
              show_add_modal ~show_node ~button:(ev##target) (fun name ->
                match React.S.value item with
                | `Context _ as item -> M.add_child m item name
                | _ -> assert false
              );
              false in
            let name = item >|~= M.Item.name in
            span [
              a ~a:[a_class ["ck-group"]; a_onclick show_group] [R.Html5.pcdata name];
              a ~a:[a_class ["ck-add-child"]; a_onclick add_clicked] [pcdata "+"];
            ]
        | `Group label -> group_label m ?adder:(W.adder group) ~show_node label in
      animated group [
        item_html;
        R.Html5.ul (
          ReactiveData.RList.map (make_tree_node_view m ~show_node) (W.children group)
        )
      ] in
    let problems, groups, done_actions =
      match ReactiveData.RList.value top with
      | [problems; groups; done_actions] -> (problems, groups, done_actions)
      | _ -> assert false in
    let heading ?adder widget =
      match W.item widget, adder with
      | `Item _, _ -> pcdata "ERROR: not a heading!"
      | `Group label, None -> h4 [pcdata label]
      | `Group label, Some adder ->
          let add_clicked ev =
            show_add_modal ~show_node ~button:(ev##target) adder;
            false in
          h4 [pcdata label; a ~a:[a_class ["ck-add-child"]; a_onclick add_clicked] [pcdata "+"]] in
    let next_children = W.children groups |> ReactiveData.RList.map make_work_actions in
    let done_children = W.children done_actions |> ReactiveData.RList.map (make_tree_node_view m ~show_node) in
    [
      div ~a:[a_class ["ck-problems"]] [
        R.Html5.ul (W.children problems |> ReactiveData.RList.map (make_tree_node_view m ~always_full:true ~show_node));
      ];
      div ~a:[a_class ["ck-next-actions"]] [
        heading groups ~adder:(fun name -> M.add_action m ~state:`Next ~name ());
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
          let date = Ck_time.string_of_unix_time log_entry.date in
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
          M.add_contact m ~name () >|= opt_show ~show_node
        );
      );
      Ck_modal.close ();
      false in
    let content =
      form ~a:[a_onsubmit submit_clicked] [
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
            M.add_action m ~state:(`Waiting_until date) ~name () >|= opt_show ~show_node
          );
          Ck_modal.close ()
        ) in
      let content = div ~a:[a_class ["ck-add-scheduled"]] [
        name_input;
        Pikaday.make ~on_select () |> fst
      ] in
      show_modal ~parent:(ev##target) [content];
      false in
    [
      h4 [pcdata "Schedule"; a ~a:[a_class ["ck-add-child"]; a_onclick add_clicked] [pcdata "+"]];
      R.Html5.ul (
        ReactiveData.RList.map (make_tree_node_view m ~show_node) tree
      )
    ]

  let review_mode_switcher ~current m =
    let item mode =
      let clicked _ev = M.set_review_mode m mode; false in
      let attrs = [a_input_type `Radio; a_onclick clicked] in
      let attrs = if mode = current then a_checked `Checked :: attrs else attrs in
      input ~a:attrs () in
    form ~a:[a_class ["ck-review-mode"]] [
      item `Done; pcdata "Done";
      item `Waiting; pcdata "Waiting";
      item `Future; pcdata "Future";
      item `Areas; pcdata "Areas";
      item `Everything; pcdata "Everything";
    ]

  let class_of_review_mode = function
    | `Done -> "ck-review-done"
    | `Waiting -> "ck-review-waiting"
    | `Future -> "ck-review-future"
    | `Areas -> "ck-review-areas"
    | `Everything -> "ck-review-everything"

  let make_tree ~show_node m = function
    | `Process tree ->
        [R.Html5.ul (ReactiveData.RList.map (make_tree_node_view m ~show_node) tree)]
    | `Review (review_mode, tree) ->
        [
          review_mode_switcher ~current:review_mode m;
          R.Html5.ul ~a:[a_class [class_of_review_mode review_mode]] (
            ReactiveData.RList.map (make_tree_node_view m ~show_node) tree
          );
        ]
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
      let clicked _ev = M.set_mode m mode; false in
      let alert_attr =
        match alert with
        | None -> React.S.const []
        | Some s ->
            s >|~= function
              | false -> []
              | true -> ["alert"] in
      let active_attr = current_tree |> React.S.map (fun t ->
        if (mode_of t) = mode then ["active"] else []
      ) in
      let cl = R.Html5.a_class (React.S.l2 (@) alert_attr active_attr) in
      let button = a ~a:[a_onclick clicked] [pcdata name] in
      li ~a:[cl] [button] in
    ul ~a:[a_class ["ck-mode-selector"]] [
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
            adder ~name ?description:None () >|= opt_show ~show_node
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
                  | `Action _ | `Contact _ | `Context _ -> pcdata ""
                  | `Project _ as item -> ul ~a:[a_class ["ck-adders"]] [
                      add_button (M.add_project m ~state:`Active ~parent:item) "+sub-project";
                      add_button (M.add_action m ~state:`Next ~parent:item ?context:None ?contact:None) "+action";
                    ]
                  | `Area _ as item -> ul ~a:[a_class ["ck-adders"]] [
                      add_button (M.add_area m ~parent:item) "+sub-area";
                      add_button (M.add_project m ~state:`Active ~parent:item) "+project";
                      add_button (M.add_action m ~state:`Next ~parent:item ?context:None ?contact:None) "+action";
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
        Ck_modal.close ();
        async ~name:"set parent" (fun () -> M.choose_candidate candidate);
        true in
      li [
        a ~a:[a_onclick clicked] [
          pcdata (M.candidate_label candidate)
        ]
      ]
    )

  let show_type_modal m ~button item =
    let make label fn item =
      let clicked _ev =
        Ck_modal.close ();
        async ~name:label (fun () -> fn m item >|= report_error ~parent:button);
        false in
      li [a ~a:[a_onclick clicked] [pcdata label]] in
    let content = ul (
      match item with
      | `Action _ as item -> [make "Convert to project" M.convert_to_project item]
      | `Project _ as p -> [make "Convert to action" M.convert_to_action p;
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
        | Some (`Contact _) -> [ck_label "A contact"]
        | Some (`Context _) -> [ck_label "A context"]
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
            | `Action _ -> [ck_label "An "; change_type "action"; ck_label " in "; title_elem; change_button]
            | `Project _ -> [ck_label "A "; change_type "project"; ck_label " in "; title_elem; change_button]
            | `Area _ -> [ck_label "An "; change_type "area"; ck_label " in "; title_elem; change_button] in
    rlist_of ~init:(React.S.value descr) descr

  let make_editable_description m item =
    let editing, set_editing = React.S.create None in
    let elements =
      editing >>~= (function
        | None ->
            item >|~= (function
              | None -> []
              | Some item ->
                  let edit _ev =
                    let descr = M.Item.description item in
                    set_editing (Some (item, descr));
                    false in
                  let append_log _ev =
                    let today = Ck_time.(string_of_user_date (Unix.gettimeofday () |> of_unix_time)) in
                    let descr =
                      match M.Item.description item with
                      | "" -> ""
                      | descr -> descr ^ "\n\n" in
                    set_editing (Some (item, Printf.sprintf "%s**%s**: " descr today));
                    false in
                  let description =
                    try
                      let raw_html = M.Item.description item |> Omd.of_string |> Omd.to_html in
                      let description = div [] in
                      let elem = Tyxml_js.To_dom.of_div description in
                      elem##innerHTML <- Js.string raw_html;
                      description
                    with ex ->
                      div [pcdata (Printexc.to_string ex)] in
                  [
                    description;
                    div ~a:[a_class ["row"]] [
                      div ~a:[a_class ["small-6"; "columns"; "ck-add-log"]] [
                        a ~a:[a_onclick append_log] [pcdata "(add log entry)"];
                      ];
                      div ~a:[a_class ["small-6"; "columns"; "ck-edit"]] [
                        a ~a:[a_onclick edit] [pcdata "(edit)"];
                      ]
                    ]
                  ]
            );
        | Some (item, descr) ->
            let cancel _ev = set_editing None; false in
            let submit_ref = ref (fun _ -> true) in
            let keydown (ev:Dom_html.keyboardEvent Js.t) =
              if ev##keyCode = 13 && Js.to_bool ev##ctrlKey then !submit_ref ev
              else true in
            let value = textarea ~a:[a_rows 5; a_onkeydown keydown]
              (pcdata descr) in
            async ~name:"focus" (fun () ->
              let elem = Tyxml_js.To_dom.of_textarea value in
              elem##focus ();
              let len = String.length descr in
              (Obj.magic elem)##setSelectionRange (len, len);
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
      Ck_modal.close () in
    let content = Pikaday.make ?initial:(due action) ~on_select () |> fst in
    show_modal ~parent:(ev##target) [content]

  let edit_context m ~show_node item ev =
    match React.S.value item with
    | Some (`Action _ as action) -> begin
        let new_contact_form =
          let name_input = input ~a:[a_name "name"; a_placeholder "New context"; a_size 20] () in
          auto_focus name_input;
          let add_context _ev =
            let input_elem = Tyxml_js.To_dom.of_input name_input in
            let name = input_elem##value |> Js.to_string |> String.trim in
            if name <> "" then async ~name:"add context" (fun () ->
              M.add_context m ~name () >>= function
              | None -> print_endline "Added item no longer exists!"; Lwt.return ()
              | Some (`Context _ as node) ->
                  Ck_modal.close ();
                  show_node node;
                  M.set_context m action node >|= report_error ~parent:(ev##target)
            );
            false in
          li [form ~a:[a_onsubmit add_context] [name_input]] in
        let contexts =
          M.candidate_contexts_for m action |> List.map (fun candidate ->
          let select _ev =
            async ~name:"set context" (fun () ->
              Ck_modal.close ();
              M.choose_candidate candidate
            );
            false in
          li [a ~a:[a_onclick select] [pcdata (M.candidate_label candidate)]]
        ) in
        let content = ul (new_contact_form :: contexts) in
        show_modal ~parent:(ev##target) [content];
        false
    end
    | _ -> false

  let edit_contact m ~show_node item ev =
    match React.S.value item with
    | Some (`Area _ | `Project _ | `Action _ as item) -> begin
        let new_contact_form =
          let name_input = input ~a:[a_name "name"; a_placeholder "New contact"; a_size 20] () in
          auto_focus name_input;
          let add_contact _ev =
            let input_elem = Tyxml_js.To_dom.of_input name_input in
            let name = input_elem##value |> Js.to_string |> String.trim in
            if name <> "" then async ~name:"add contact" (fun () ->
              M.add_contact m ~name () >>= function
              | None -> print_endline "Added item no longer exists!"; Lwt.return ()
              | Some (`Contact _ as node) ->
                  Ck_modal.close ();
                  show_node node;
                  M.set_contact m item (Some node) >|= report_error ~parent:(ev##target)
            );
            false in
          li [form ~a:[a_onsubmit add_contact] [name_input]] in
        let contacts =
          M.candidate_contacts_for m item |> List.map (fun candidate ->
          let select _ev =
            async ~name:"set contact" (fun () ->
              Ck_modal.close ();
              M.choose_candidate candidate
            );
            false in
          li [a ~a:[a_onclick select] [pcdata (M.candidate_label candidate)]]
        ) in
        let content = ul (new_contact_form :: contacts) in
        show_modal ~parent:(ev##target) [content];
        false
    end
    | _ -> false

  let unit_options = Ck_time.([| Day; Week; Month; Year |])
  let edit_repeat m action ev =
    let open Ck_time in
    let init =
      match M.Item.action_repeat action with
      | None ->
          make_repeat 1 Week ~from:(
            match due action with
            | None -> Unix.gettimeofday () |> Ck_time.of_unix_time
            | Some date -> date
          )
      | Some r -> r in
    let n_input = input ~a:[a_name "n"; a_size 2; a_input_type `Text; a_value (string_of_int init.repeat_n)] () in
    auto_focus n_input;
    let unit_input =
      let unit_option u =
        let attrs =
          if u = init.repeat_unit then [a_selected `Selected] else [] in
        option ~a:attrs (pcdata ((string_of_time_unit u) ^ "s")) in
      select (unit_options |> Array.map unit_option |> Array.to_list) in
    let on_select date =
      let input_elem = To_dom.of_input n_input in
      let n =
        try
          let n = input_elem##value |> Js.to_string |> String.trim |> int_of_string in
          if n > 0 then Some n else None
        with Failure _ -> None in
      match n with
      | None -> ()
      | Some n ->
      let unit_input = Tyxml_js.To_dom.of_select unit_input in
      let units = unit_options.(unit_input##selectedIndex) in
      async ~name:"edit_repeat" (fun () ->
        Some (make_repeat n units ~from:date)
        |> M.set_repeat m action
      );
      Ck_modal.close () in
    let picker_elem, pikaday = Pikaday.make ~initial:init.repeat_from ~on_select () in
    let submit _ev =
      on_select (Pikaday.get_date pikaday);
      false in
    [
      form ~a:[a_onsubmit submit; a_class ["ck-repeat"]] [
        div ~a:[a_class ["ck-repeat-interval"]] [
          ck_label "Every";
          n_input;
          unit_input;
          ck_label "from";
        ];
      ];
      picker_elem
    ]
    |> show_modal ~parent:(ev##target);
    false

  let clear_repeat m action =
    let clear _ev =
      async ~name:"clear_repeat" (fun () -> M.set_repeat m action None);
      false in
    a ~a:[a_onclick clear; a_class ["delete"]] [entity "cross"]

  let make_conflicts m item =
    item >|~= function
    | None -> []
    | Some item ->
    match M.Item.conflicts item with
    | [] -> []
    | ms ->
        let clear_conflicts _ev =
          async ~name:"clear_conflicts" (fun () -> M.clear_conflicts m item);
          false in
        [
          div ~a:[a_class ["ck-conflicts"]] [
            a ~a:[a_class ["close"]; a_onclick clear_conflicts] [pcdata "×"];
            h4 [pcdata "Merge conflicts"];
            ul (
              ms |> List.map (fun msg -> li [pcdata msg])
            )
          ]
        ]

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
      | Some item -> M.Item.ctime item |> Ck_time.string_of_unix_time in
    let description = make_editable_description m item in
    let title = 
      div ~a:[R.Html5.a_class title_cl] [
        R.Html5.span ~a:[a_class ["ck-toggles"]] (make_state_toggles m item);
        R.Html5.div ~a:[a_class ["inline"]] (make_editable_title m item);
      ] in
    let waiting_for =
      item >|~= (function
        | Some (`Action _ as action) ->
            let waiting =
              match M.Item.action_state action with
              | `Waiting_until time ->
                  let clicked ev = edit_date ~ev m action; false in
                  [ck_label "Waiting until "; a ~a:[a_onclick clicked] [pcdata (Ck_time.string_of_user_date time)]]
              | _ -> [] in
            let repeating, clear =
              match M.Item.action_repeat action with
              | None -> "(never)", []
              | Some repeat -> Ck_time.string_of_repeat repeat, [clear_repeat m action] in
            [
              div waiting;
              div (
                [
                  ck_label "Repeats: ";
                  a ~a:[a_onclick (edit_repeat m action)] [pcdata repeating];
                ] @ clear
              )
            ]
        | _ -> []
      ) |> rlist_of in
    let contact =
      match details.M.details_contact with
      | None -> []
      | Some signal ->
          let label = item >|~= (function
            | Some (`Action _ as a) when M.Item.action_state a = `Waiting_for_contact -> "Waiting for: "
            | _ -> "Contact: "
          ) in
          let value =
            signal >|~= (fun opt_contact ->
              let contact_name =
                match opt_contact with
                | None -> pcdata "(no contact)"
                | Some c -> render_item m ~show_node (c :> M.Item.generic) in
              [
                contact_name;
                a ~a:[a_onclick (edit_contact m ~show_node item)] [pcdata " (change)"]
              ]
            ) |> rlist_of in
          [
            span ~a:[a_class ["ck-label"]] [R.Html5.pcdata label];
            R.Html5.span value;
          ] in
    let context =
      details.M.details_context >|~= (function
        | None -> []            (* Item type doesn't have contexts *)
        | Some opt_context ->
            let context_name =
              match opt_context with
              | None -> pcdata "(no context)"
              | Some c -> render_item m ~show_node (c :> M.Item.generic) in
            [
              ck_label "Context: ";
              context_name;
              a ~a:[a_onclick (edit_context m ~show_node item)] [pcdata " (change)"]
            ]
      ) |> rlist_of in
    let contents =
      [
        div [
          R.Html5.span (make_parent_details m ~show_node details);
        ];
        R.Html5.div context;
        R.Html5.div (make_conflicts m item |> rlist_of);
      ] @ contact @ [
        R.Html5.div waiting_for;
        R.Html5.ul ~a:[a_class ["ck-groups"]] children;
        make_child_adder m ~show_node item;
        description;
        div [
          ck_label ("Created " ^ ctime);
          a ~a:[a_onclick delete_clicked; a_class ["ck-delete"]] [pcdata " (delete)"];
        ];
      ] in
    (title, div contents)

  let search_create_bar m ~show_node =
    let my_input = ref None in
    let my_form = ref None in
    let value, set_value = React.S.create "" in
    let cl = value |> React.S.map (function
      | "" -> ["ck-menu-hidden"]
      | _ -> ["ck-menu-dropdown"]
    ) in
    let close () =
      begin match !my_input with
      | None -> assert false
      | Some input -> (To_dom.of_input input)##value <- Js.string "" end;
      set_value "" in
    let oninput ev =
      Js.Opt.iter (ev##target) (fun input ->
        Js.Opt.iter (Dom_html.CoerceTo.input input) (fun input ->
          let v = input##value |> Js.to_string |> String.trim in
          begin match React.S.value value, v with
          | "", "" -> ()
          | "", _ ->
              let f = match !my_form with None -> assert false | Some f -> f in
              Ck_modal.show ~close (To_dom.of_form f)
          | _, "" -> Ck_modal.close ()
          | _, _ -> () end;
          set_value v
        )
      );
      true in
    let input_box = input ~a:[a_oninput oninput; a_name "v"; a_placeholder "Add item"; a_size 20; a_autocomplete `Off] () in
    let add adder _ev =
      begin match React.S.value value with
      | "" -> ()
      | name ->
          Ck_modal.close ();
          async ~name:"add-action-top" (fun () ->
            adder m ~name () >|= opt_show ~show_node
          )
      end;
      false in
    let submit = add (M.add_action ~state:`Next ?context:None ?contact:None ?parent:None ?description:None) in
    my_input := Some input_box;
    let action adder label =
      li [a ~a:[a_onclick (add adder)] [pcdata label]] in
    let f = form ~a:[a_onsubmit submit; a_class ["ck-main-entry"]] [
      input_box;
      div ~a:[R.Html5.a_class cl] [
        ul [
          action (M.add_action ~state:`Next ?context:None ?contact:None ?parent:None ?description:None) "Add action";
          action (M.add_project ~state:`Active ?parent:None ?description:None) "Add project";
          action (M.add_area ?parent:None ?description:None) "Add area";
          action (M.add_contact) "Add contact";
          action (M.add_context) "Add context";
        ]
      ]
    ] in
    my_form := Some f;
    f

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
                  (Ck_time.string_of_unix_time log_entry.Git_storage_s.Log_entry.date)
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
        div ~a:[a_class ["medium-4"; "columns"]] [
          search_create_bar m ~show_node;
          div ~a:[a_class ["ck-actions"]] [
            a ~a:[a_onclick (fun _ -> show_history (); false)] [pcdata "Show history"];
            a ~a:[a_onclick (fun _ -> close_all (); false)] [pcdata "Close all"];
          ]
        ]
      ];
      div ~a:[a_class ["row"]] [
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
