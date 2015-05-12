open OUnit
open Lwt

module StringList = OUnitDiff.ListSimpleMake(struct
  type t = string
  let compare = String.compare
  let pp_printer = Format.pp_print_string
  let pp_print_sep = Format.pp_print_newline
end)

(* let () = Log.(set_log_level INFO) *)

let () = Random.self_init ()
(* let () = Random.init 8 *)

module Queue = Lwt_pqueue.Make(struct
  type t = (float * unit Lwt.u)
  let compare a b =
    compare (fst a) (fst b)
end)

let debug fmt = Printf.ksprintf ignore fmt

module Test_clock = struct
  let schedule = ref Queue.empty
  let time = ref 0.0

  let now () = !time
  let sleep delay =
    assert (delay >= 0.0);
    let result, waker = Lwt.task () in
    schedule := !schedule |> Queue.add (!time +. delay, waker);
    result

  let async ~name f =
    let (_ : unit Lwt.t) =
      catch (fun () -> sleep 0.0 >>= f)
        (fun ex ->
          Printf.printf "Async error from '%s': %s\n" name (Printexc.to_string ex);
          exit 1) in
    ()

  let rec run_to t =
    debug "run_to %.2f\n" t;
    match Queue.lookup_min !schedule with
    | Some (wake_time, w) when wake_time <= t ->
        schedule := !schedule |> Queue.remove_min;
        time := wake_time;
        debug "time = %.2f (waking)\n" !time;
        Lwt.wakeup w ();
        run_to t
    | _ ->
        time := t;
        debug "time = %.2f\n" !time

  let reset () =
    schedule := Queue.empty;
    time := 0.0
end

module Key = struct
  module Id = Ck_id
  type t = Ck_id.t * string
  let id = fst
  let show = snd
  let compare a b =
    match compare (snd a) (snd b) with
    | 0 -> compare (fst a) (fst b)
    | r -> r
end

module Test_rpc = struct
  include Cohttp_lwt_unix.Client
  let get ?ctx:_ ?headers:_ _ = failwith "GET"
  let post ?ctx:_ ?body:_ ?chunked:_ ?headers:_ _ = failwith "POST"
end

(*
module Store = Irmin.Basic(Irmin_unix.Irmin_git.FS)(Irmin.Contents.String)
let _ = Unix.system "rm -rf /tmp/test_db/.git"
let () = Irmin_unix.install_dir_polling_listener 1.0
let config = Irmin_unix.Irmin_git.config ~root:"/tmp/test_db" ()
*)
let config = Irmin_mem.config ()

let task s =
  let date = Test_clock.now () |> Int64.of_float in
  Irmin.Task.create ~date ~owner:"User" s

let format_list l = "[" ^ (String.concat "; " l) ^ "]"

type node = N of string * node list
let n name children = N (name, children)

let assert_str_equal = assert_equal ~printer:(fun x -> x)

module Test_repo (Store : Irmin.BASIC with type key = string list and type value = string) = struct
  module Git = Git_storage.Make(Store)
  module ItemMap = Map.Make(Key)
  module Slow = Slow_set.Make(Test_clock)(Key)(ItemMap)
  module M = Ck_model.Make(Test_clock)(Git)(struct type t = unit end)(Test_rpc)
  module W = M.Widget
  module Rev = Ck_rev.Make(Git)
  module Up = Ck_update.Make(Git)(Test_clock)(Rev)

  (* Workaround for Irmin creating empty directories *)
  let filter_list staging path =
    Git.Staging.list staging path
    >>= Lwt_list.filter_s (fun p ->
      Git.Staging.list staging p >|= function
        | [] -> false
        | _ -> true
    )

  let assert_git_tree_equals ~msg expected actual =
    Git.Commit.checkout expected >>= fun expected ->
    Git.Commit.checkout actual >>= fun actual ->
    let rec check path =
      filter_list expected path >>= fun e_paths ->
      filter_list actual path >>= fun a_paths ->
      let msg = String.concat "/" (msg :: path) in
      let printer paths =
        paths |> List.map (String.concat "/") |> String.concat "," in
      assert_equal ~msg ~printer e_paths a_paths;
      a_paths |> Lwt_list.iter_s check in
    check []

  let name_of widget =
    match W.item widget with
    | `Item item ->
        let item = React.S.value item in
        M.Item.name item
    | `Group name -> name

  let rec get_tree rl =
    ReactiveData.RList.value rl
    |> List.map (fun widget ->
      let name = name_of widget in
      let children = get_tree (W.children widget) in
      let str =
        match W.state widget |> React.S.value with
        | `New -> "+" ^ name
        | `Init -> "@" ^ name
        | `Removed _ -> "-" ^ name
        | `Current -> name
      in
      N (str, children)
    )

  let assert_tree ?label expected actual =
    let msg =
      match label with
      | Some l -> l
      | None -> Ck_time.string_of_unix_time !Test_clock.time in
    let rec printer items = items
      |> List.map (fun (N (name, children)) -> name ^ "(" ^ printer children ^ ")")
      |> String.concat ", " in
    debug "Expecting: %s\n" (printer expected);
    let actual =
      match get_tree actual with
      | N (("Problems" | "@Problems"), []) :: xs -> xs   (* Remove Problems if empty, as the GUI does *)
      | xs ->xs in
    assert_equal ~msg ~printer expected actual

  let rec lookup path widgets =
    match path with
    | [] -> assert false
    | (p::ps) ->
        let step =
          try
            ReactiveData.RList.value widgets
            |> List.find (fun widget ->
              name_of widget = p
            )
          with Not_found -> Ck_utils.bug "Node '%s' not found" p in
        match ps with
        | [] ->
            begin match W.item step with
            | `Item s -> React.S.value s
            | `Group label -> Ck_utils.bug "Not an item '%s'" label end;
        | ps -> lookup ps (W.children step)

  let random_state ~common ~random repo =
    (* Bias towards similar choices within a set *)
    let rand_int n =
      let r = Random.State.int random (3 * n) in
      if r >= n then common mod n else r in
    let rand_string n = string_of_int (rand_int n) in
    let rand_time n = float_of_int (rand_int n) in
    let rand_date () = Ck_time.make ~year:(2000 + rand_int 3) ~month:(rand_int 3) ~day:(rand_int 3) in

    let choose options =
      options.(rand_int (Array.length options)) in

    Git.Repository.empty repo >>= fun s ->

    let make_n n dir fn =
      let used = ref [None] in
      let n_possible = ref (2 * n) in
      let possible_uuids = Array.init !n_possible string_of_int in
      let unused_uuid () =
        let i = rand_int (!n_possible) in
        let uuid = possible_uuids.(i) in
        possible_uuids.(i) <- possible_uuids.(!n_possible - 1);
        decr n_possible;
        uuid in
      let rec aux todo = function
        | 0 -> todo
        | i ->
            let id = unused_uuid () in
            let uuid = Ck_id.of_string id in
            used := Some uuid :: !used;
            let str = fn
              ~uuid
              ~name:("n-" ^ rand_string 3)
              ~description:(rand_string 3)
              ~ctime:(rand_time 3) in
            aux ((id, str) :: todo) (i - 1) in
      aux [] n
      |> Lwt_list.iter_s (fun (uuid, str) ->
          Git.Staging.update s [dir; uuid] str
      ) >|= fun () ->
      Array.of_list !used in

    let random_contact ~uuid:_ ~name ~description ~ctime =
      Ck_disk_node.make_contact ~name ~description ~ctime ()
      |> Ck_disk_node.contact_to_string in

    let random_context ~uuid:_ ~name ~description ~ctime =
      Ck_disk_node.make_context ~name ~description ~ctime ()
      |> Ck_disk_node.context_to_string in

    let areas = ref [None] in
    let projects = ref [] in
    let random_apa ~contacts ~contexts ~uuid ~name ~description ~ctime =
      let contact = choose contacts in
      begin match rand_int 3 with
      | 0 ->
          let parent = choose (Array.of_list !areas) in
          areas := Some uuid :: !areas;
          Ck_disk_node.make_area ?contact ~name ~description ~ctime ?parent ()
      | 1 ->
          let parent = choose (Array.of_list (!areas @ !projects)) in
          projects := Some uuid :: !projects;
          Ck_disk_node.make_project ?contact ~name ~description ~ctime ~state:(choose [| `Active; `SomedayMaybe; `Done |]) ?parent ()
      | _ ->
          let context = choose contexts in
          let parent = choose (Array.of_list (!areas @ !projects)) in
          let state =
            match contact with
            | None -> choose [| `Next; `Waiting; `Waiting_until (rand_date ()); `Future; `Done |]
            | Some _ -> choose [| `Next; `Waiting; `Waiting_until (rand_date ()); `Waiting_for_contact; `Future; `Done |] in
          let a = Ck_disk_node.make_action ?contact ?context ~name ~description ~ctime ~state ?parent () in
          match state with
          | `Done -> a
          | _ ->
              if rand_int 5 = 0 then
                Ck_disk_node.with_repeat a (Some (
                  Ck_time.make_repeat ~from:(rand_date ()) (1 + rand_int 2) (choose Ck_time.([| Day ; Week ; Month ; Year |]))
                ))
              else a
      end
      |> Ck_disk_node.to_string in

    make_n (rand_int 2) "contact" random_contact >>= fun contacts ->
    make_n (rand_int 2) "context" random_context >>= fun contexts ->
    make_n (rand_int 5) "db" (random_apa ~contacts ~contexts) >>= fun _nodes ->
    return s
end

let expect_tree s =
  match React.S.value s with
  | `Process rl | `Work (_, rl) -> rl
  | _ -> assert false

let expect_area = function
  | `Area _ as x -> x
  | _ -> assert false

let expect_some = function
  | None -> assert false
  | Some x -> x

let run_with_exn fn =
  try
    fn () |> Lwt_main.run
  with ex ->
    Printexc.print_backtrace stderr;
    raise ex

let expect_action item =
  match item with
  | `Action _ as x -> x
  | _ -> assert_failure "Not an action!"

let expect_project item =
  match item with
  | `Project _ as x -> x
  | _ -> assert_failure "Not a project!"

let day d = Ck_time.make ~year:2015 ~month:4 ~day:d

let suite = 
  "cue-keeper">:::[
    "delay_rlist">:: (fun () ->
      let module Store = Irmin.Basic(Irmin_mem.Make)(Irmin.Contents.String) in
      let module T = Test_repo(Store) in
      let open T in
      Test_clock.reset ();
      let src, set_src = React.S.create ~eq:(ItemMap.equal (=)) ItemMap.empty in
      let set items =
        List.fold_left (fun acc i ->
          ItemMap.add i i acc
        ) ItemMap.empty items
        |> set_src in
      let rename o n =
        React.S.value src
        |> ItemMap.remove o
        |> ItemMap.add n n
        |> set_src in

      let eqd rl expected =
        (* Printf.printf "Expecting: %s\n" (String.concat ", " expected); *)
        let actual =
          ItemMap.fold (fun _k item acc ->
            let (_id, b) = Slow_set.data item in
            let s = match React.S.value (Slow_set.state item) with
              | `New -> "+" ^ b
              | `Init -> "@" ^ b
              | `Current -> b
              | `Removed _ -> "-" ^ b in
            s :: acc
          ) (React.S.value rl) []
          |> List.rev in
        let msg = Printf.sprintf "t=%.2f" !Test_clock.time in
        assert_equal ~msg ~printer:format_list expected actual in
      let dst = Slow.make ~eq:(=) ~delay:1.0 src in
      eqd dst [];

      let n name =
        let id = float_of_string name *. 10. |> Printf.sprintf "%g" |> Ck_id.of_string in
        (id, name) in

      let a0  = n "0" in
      let a1  = n "1" in
      let a15 = n "1.5" in
      let a2  = n "2" in
      let a3  = n "3" in
      let a35 = n "3.5" in
      let a4  = n "4" in

      set [a1];
      eqd dst ["+1"];

      set [];
      eqd dst ["-1"];
      Test_clock.run_to 2.0;
      eqd dst [];

      set [a1; a2; a3];
      set [a1; a3];   (* Remove 2 at t=2.0 *)
      eqd dst ["+1"; "-2"; "+3"];

      set [a0; a1; a15; a3];
      eqd dst ["+0"; "+1"; "+1.5"; "-2"; "+3"];

      set [a0; a1; a15; a3; a35];
      eqd dst ["+0"; "+1"; "+1.5"; "-2"; "+3"; "+3.5"];
      Test_clock.run_to 2.1;
      eqd dst ["+0"; "+1"; "+1.5"; "-2"; "+3"; "+3.5"];

      set [a1; a15; a3; a35];
      eqd dst ["-0"; "+1"; "+1.5"; "-2"; "+3"; "+3.5"];
      Test_clock.run_to 3.0;
      eqd dst ["-0"; "1"; "1.5"; "3"; "3.5"];
      Test_clock.run_to 3.1;
      eqd dst ["1"; "1.5"; "3"; "3.5"];

      set [a1; a15; a3];
      eqd dst ["1"; "1.5"; "3"; "-3.5"];
      Test_clock.run_to 4.1;
      eqd dst ["1"; "1.5"; "3"];

      set [a15; a3];
      eqd dst ["-1"; "1.5"; "3"];
      Test_clock.run_to 5.1;
      eqd dst ["1.5"; "3"];

      set [a0; a3];
      eqd dst ["+0"; "-1.5"; "3"];

      Test_clock.run_to 6.1;
      eqd dst ["0"; "3"];

      set [a4];
      eqd dst ["-0"; "-3"; "+4"];
      Test_clock.run_to 7.1;
      eqd dst ["4"];

      set [a0; a1; a2; a3];
      Test_clock.run_to 10.0;
      eqd dst ["0"; "1"; "2"; "3"];

      let two = (fst a2, "two") in
      rename a2 two;
      eqd dst ["0"; "1"; "-2"; "3"; "+two"];
      rename a1 (fst a1, "1.5");
      eqd dst ["0"; "1.5"; "-2"; "3"; "+two"];
      rename a3 (fst a3, "2.5");
      eqd dst ["0"; "1.5"; "-2"; "2.5"; "+two"];

      Test_clock.run_to 12.0;
      eqd dst ["0"; "1.5"; "2.5"; "two"];
    );

    "model">:: (fun () ->
      let module Store = Irmin.Basic(Irmin_mem.Make)(Irmin.Contents.String) in
      let module T = Test_repo(Store) in
      let open T in
      run_with_exn begin fun () ->
        Test_clock.reset ();
        let run_to_day d =
          day d |> Ck_time.unix_time_of |> Test_clock.run_to in
        let wait s =
          Test_clock.run_to (!Test_clock.time +. s) in
        run_to_day 0;
        Git.make config task >>= M.make >>= fun m ->
        M.set_mode m `Process;
        let process_tree = M.tree m |> expect_tree in
        let job = lookup ["Job"] process_tree |> expect_area in

        M.set_mode m `Work;
        let next_actions = M.tree m |> expect_tree in

        M.add_action m ~state:`Next ~parent:job ~name:"Write unit tests" () >>= fun _ ->

        (* Initially, we have a single Next action *)
        next_actions |> assert_tree ~label:"start" [
          n "@Next actions" [
            n "@Reading" [
              n "@Start using CueKeeper" [
                n "@Read wikipedia page on GTD" []
              ];
            ];
            n "+(no context)" [
              n "@Job" [
                n "@Write unit tests" []
              ]
            ];
          ];
          n "@Recently completed" []
        ];

        let read = lookup ["Next actions"; "Reading"; "Start using CueKeeper"; "Read wikipedia page on GTD"] next_actions in
        let start_using_ck = lookup ["Next actions"; "Reading"; "Start using CueKeeper"] next_actions |> expect_project in
        let units = lookup ["Next actions"; "(no context)"; "Job"; "Write unit tests"] next_actions |> expect_action in

        (* After changing it to Waiting, it disappears from the list. *)
        M.set_action_state m units `Waiting >>= fun () ->
        M.delete m read >>= function
        | `Error x -> failwith x
        | `Ok () ->
        next_actions |> assert_tree ~label:"waiting" [
          n "@Problems" [
            n "+Active project with no next action" [
              n "@Start using CueKeeper" []
            ];
          ];
          n "@Next actions" [
            n "-Reading" [
              n "@Start using CueKeeper" [
                n "@Read wikipedia page on GTD" []
              ];
            ];
            n "-(no context)" [
              n "@Job" [
                n "@Write unit tests" []
              ]
            ];
          ];
          n "@Recently completed" [];
        ];
        M.set_project_state m start_using_ck `SomedayMaybe >>= fun () ->
        wait 2.0;
        next_actions |> assert_tree ~label:"empty" [
          n "Next actions" [];
          n "Recently completed" [];
        ];

        M.add_action m ~state:`Next ~parent:job ~name:"GC unused signals" () >>= function
        | None | Some (`Area _ | `Project _) -> assert false
        | Some (`Action _ as gc) ->
        M.add_context m ~name:"Coding" () >>= function
        | None -> assert false
        | Some coding ->
        M.set_context m gc coding >>= function
        | `Error msg -> assert_failure msg
        | `Ok () ->
        M.set_context m units coding >>= function
        | `Error msg -> assert_failure msg
        | `Ok () ->
        next_actions |> assert_tree [
          n "Next actions" [
            n "+Coding" [
              n "@Job" [
                n "@GC unused signals" [];
              ]
            ];
            n "-(no context)" [
              n "@Job" [
                n "@GC unused signals" [];
              ]
            ];
          ];
          n "Recently completed" [];
        ];

        (* Get the updated units. *)
        let live_units = M.details m units in
        let units = React.S.value (live_units.M.details_item) |> expect_some |> expect_action in
        assert (M.Item.action_state units <> `Next);
        (* Changing back to Next makes it reappear *)
        M.set_action_state m units `Next >>= fun () ->
        next_actions |> assert_tree [
          n "Next actions" [
            n "+Coding" [
              n "@Job" [
                n "@GC unused signals" [];
                n "+Write unit tests" []
              ];
            ];
            n "-(no context)" [
              n "@Job" [
                n "@GC unused signals" [];
              ]
            ];
          ];
          n "Recently completed" [];
        ];

        let units = React.S.value (live_units.M.details_item) |> expect_some |> expect_action in
        M.set_action_state m units `Waiting >>= fun () ->
        wait 2.0;
        next_actions |> assert_tree [
          n "Next actions" [
            n "Coding" [
              n "Job" [
                n "GC unused signals" [];
              ]
            ]
          ];
          n "Recently completed" [];
        ];

        let units = React.S.value (live_units.M.details_item) |> expect_some |> expect_action in
        M.set_action_state m units (`Waiting_until (day 5)) >>= fun () ->
        next_actions |> assert_tree [
          n "Next actions" [
            n "Coding" [
              n "Job" [
                n "GC unused signals" [];
              ]
            ]
          ];
          n "Recently completed" [];
        ];

        run_to_day 5;
        next_actions |> assert_tree [
          n "Next actions" [
            n "Coding" [
              n "Job" [
                n "GC unused signals" [];
                n "+Write unit tests" [];
              ]
            ];
            n "Review" [
              n "Weekly review" [];
            ]
          ];
          n "Recently completed" [];
        ];

        let review = lookup ["Next actions"; "Review"; "Weekly review"] next_actions |> expect_action in
        M.delete m review >>= function
        | `Error msg -> assert_failure msg
        | `Ok () ->
        let units = React.S.value (live_units.M.details_item) |> expect_some |> expect_action in
        M.set_action_state m units (`Waiting_until (day 6)) >>= fun () ->
        M.add_action m ~state:(`Waiting_until (day 7)) ~parent:job ~name:"Implement scheduing" () >>= fun _ ->
        next_actions |> assert_tree [
          n "Next actions" [
            n "Coding" [
              n "Job" [
                n "GC unused signals" [];
                n "-Write unit tests" [];
              ]
            ];
            n "-Review" [
              n "Weekly review" [];
            ]
          ];
          n "Recently completed" [];
        ];

        run_to_day 6;
        next_actions |> assert_tree [
          n "Next actions" [
            n "Coding" [
              n "Job" [
                n "GC unused signals" [];
                n "+Write unit tests" [];
              ]
            ]
          ];
          n "Recently completed" [];
        ];

        run_to_day 7;
        wait 0.5;
        next_actions |> assert_tree [
          n "Next actions" [
            n "Coding" [
              n "Job" [
                n "GC unused signals" [];
                n "Write unit tests" [];
              ]
            ];
            n "+(no context)" [
              n "@Job" [
                n "@Implement scheduing" [];
              ]
            ];
          ];
          n "Recently completed" [];
        ];

        (* Renaming a group *)
        M.set_name m job "Dev" >>= fun () ->
        next_actions |> assert_tree [
          n "Next actions" [
            n "Coding" [
              n "Dev" [
                n "GC unused signals" [];
                n "Write unit tests" [];
              ]
            ];
            n "+(no context)" [
              n "Dev" [
                n "@Implement scheduing" [];
              ]
            ];
          ];
          n "Recently completed" [];
        ];
        let job = lookup ["Next actions"; "Coding"; "Dev"] next_actions |> expect_area in

        (* Rename conflict (e.g. two edits in different tabs *)
        M.add_action m ~state:`Next ~parent:job ~name:"Implement merging" () >>= fun conflict ->
        let conflict = expect_action (expect_some conflict) in
        let live_conflict = M.details m conflict in
        M.set_name m conflict "Test conflicts" >>= fun () ->
        wait 2.0;
        M.set_name m conflict "Fix merging" >>= fun () ->
        next_actions |> assert_tree ~label:"conflicts" [
          n "Problems" [
            n "+Unread merge conflicts report" [
              n "@Fix merging" []
            ]
          ];
          n "Next actions" [
            n "Coding" [
              n "Dev" [
                n "GC unused signals" [];
                n "Write unit tests" [];
              ]
            ];
            n "(no context)" [
              n "Dev" [
                n "+Fix merging" [];
                n "Implement scheduing" [];
                n "-Fix merging" [];
              ]
            ];
          ];
          n "Recently completed" [
          ];
        ];
        let conflict = React.S.value live_conflict.M.details_item |> expect_some in
        M.Item.conflicts conflict |> assert_equal ["Discarded change Implement merging -> Test conflicts"];
        M.clear_conflicts m conflict >>= fun () ->
        let conflict = React.S.value live_conflict.M.details_item |> expect_some in
        M.delete m conflict >>= function
        | `Error msg -> assert_failure msg
        | `Ok () ->
        wait 2.0;

        (* Add parent project*)
        M.add_project m ~parent:job ~state:`Active ~name:"Release CueKeeper" () >>= fun release ->
        let release = expect_some release |> expect_project in
        let units = React.S.value (live_units.M.details_item) |> expect_some |> expect_action in
        let candidates = M.candidate_parents_for m units in
        let set_parent = List.find (fun p -> M.candidate_label p = "» Release CueKeeper") candidates in
        M.choose_candidate set_parent >>= fun () ->

        (* Mark as Done *)
        let units = React.S.value (live_units.M.details_item) |> expect_some |> expect_action in
        M.set_action_state m units `Done >>= fun () ->
        M.set_project_state m release `Done >>= fun () ->
        wait 2.0;
        next_actions |> assert_tree ~label:"done" [
          n "Next actions" [
            n "Coding" [
              n "Dev" [
                n "GC unused signals" [];
              ]
            ];
            n "(no context)" [
              n "Dev" [
                n "Implement scheduing" [];
              ]
            ];
          ];
          n "Recently completed" [
            n "Release CueKeeper" [];
          ]
        ];
        M.delete_done m >>= fun () ->
        wait 2.0;
        next_actions |> assert_tree ~label:"done deleted" [
          n "Next actions" [
            n "Coding" [
              n "Dev" [
                n "GC unused signals" [];
              ]
            ];
            n "(no context)" [
              n "Dev" [
                n "Implement scheduing" [];
              ]
            ];
          ];
          n "Recently completed" []
        ];

        (* Check history *)
        M.enable_log m >>= fun live_log ->
        let log = ReactiveData.RList.value live_log |> List.map Slow_set.data in
        log
        |> List.map (fun entry -> entry.Git_storage_s.Log_entry.msg |> List.hd)
        |> StringList.assert_equal
        [
          "Delete done items";
          "Release CueKeeper: active -> done";
          "Write unit tests: waiting until 2015-05-06 (Wed) -> done";
          "Write unit tests: move under Release CueKeeper";
          "Create Release CueKeeper";
          "Fix merging: deleted";
          "Fix merging: clear conflicts";
          "Merge";
          "Implement merging: rename to Fix merging";
          "Implement merging: rename to Test conflicts";
          "Create Implement merging";
          "Job: rename to Dev";
          "Create Implement scheduing";
          "Write unit tests: waiting until 2015-05-05 (Tue) -> waiting until 2015-05-06 (Wed)";
          "Weekly review: deleted";
          "Write unit tests: waiting -> waiting until 2015-05-05 (Tue)";
          "Write unit tests: next -> waiting";
          "Write unit tests: waiting -> next";
          "Write unit tests: context now Coding";
          "GC unused signals: context now Coding";
          "Add context Coding";
          "Create GC unused signals";
          "Start using CueKeeper: active -> someday/maybe";
          "Read wikipedia page on GTD: deleted";
          "Write unit tests: next -> waiting";
          "Create Write unit tests";
          "Initialise repository";
        ];

        let delete_fix_merging = List.nth log 5 in
        assert_str_equal "Fix merging: deleted" (List.hd delete_fix_merging.Git_storage_s.Log_entry.msg);
        M.revert m delete_fix_merging >>= function
        | `Error msg -> assert_failure msg
        | `Ok () ->

        let revert_entry = ReactiveData.RList.value live_log |> List.hd |> Slow_set.data in
        revert_entry.Git_storage_s.Log_entry.msg |> List.hd |> assert_str_equal "Revert \"Fix merging: deleted\"";
        wait 2.0;
        next_actions |> assert_tree ~label:"revert" [
          n "Next actions" [
            n "Coding" [
              n "Dev" [
                n "GC unused signals" [];
              ]
            ];
            n "(no context)" [
              n "Dev" [
                n "Fix merging" [];
                n "Implement scheduing" [];
              ]
            ];
          ];
          n "Recently completed" []
        ];

        M.revert m revert_entry >>= function
        | `Error msg -> assert_failure msg
        | `Ok () ->
        wait 2.0;
        next_actions |> assert_tree ~label:"revert revert" [
          n "Next actions" [
            n "Coding" [
              n "Dev" [
                n "GC unused signals" [];
              ]
            ];
            n "(no context)" [
              n "Dev" [
                n "Implement scheduing" [];
              ]
            ];
          ];
          n "Recently completed" []
        ];

        return ()
      end
    );

    "time">:: (fun () ->
      let open Ck_time in
      let assert_less b a =
        assert_equal (compare b (of_tm (tm_of b))) 0;
        assert_equal (compare b (of_unix_time (unix_time_of b))) 0;
        assert_equal (compare b b) 0;
        assert_equal (compare a b) (-1);
        assert_equal (compare b a) 1 in
      make ~year:2000 ~month:10 ~day:5 |> assert_less (make ~year:2001 ~month:10 ~day:5);
      make ~year:2000 ~month:10 ~day:5 |> assert_less (make ~year:2000 ~month:11 ~day:5);
      make ~year:2000 ~month:10 ~day:5 |> assert_less (make ~year:2000 ~month:10 ~day:6);
      make ~year:2000 ~month:11 ~day:1 |> assert_less (make ~year:2000 ~month:10 ~day:33);

      make ~year:2015 ~month:04 ~day:9 |> string_of_user_date |> assert_str_equal "2015-05-09 (Sat)";
      let apr30 = make ~year:2015 ~month:4 ~day:0 in
      let may1st = make ~year:2015 ~month:4 ~day:1 in

      let rep = make_repeat 2 Day ~from:(make ~year:2015 ~month:3 ~day:9) in
      rep.repeat_from |> string_of_user_date |> assert_str_equal "2015-04-09 (Thu)";
      next_repeat rep ~now:rep.repeat_from |> string_of_user_date |> assert_str_equal "2015-04-11 (Sat)";
      next_repeat rep ~now:apr30 |> string_of_user_date |> assert_str_equal "2015-05-01 (Fri)";
      next_repeat rep ~now:may1st |> string_of_user_date |> assert_str_equal "2015-05-03 (Sun)";

      let rep = make_repeat 1 Week ~from:(make ~year:2015 ~month:3 ~day:9) in
      next_repeat rep ~now:rep.repeat_from |> string_of_user_date |> assert_str_equal "2015-04-16 (Thu)";

      let rep = make_repeat 3 Month ~from:(make ~year:2014 ~month:11 ~day:30) in
      next_repeat rep ~now:rep.repeat_from |> string_of_user_date |> assert_str_equal "2015-03-30 (Mon)";
      let rep = make_repeat 2 Month ~from:(make ~year:2014 ~month:11 ~day:30) in
      next_repeat rep ~now:rep.repeat_from |> string_of_user_date |> assert_str_equal "2015-02-28 (Sat)";

      let rep = make_repeat 10 Year ~from:(make ~year:2000 ~month:2 ~day:1) in
      next_repeat rep ~now:apr30 |> string_of_user_date |> assert_str_equal "2020-03-01 (Sun)";
    );

    "merging">:: (fun () ->
      let seed = Random.int (1 lsl 28) in
      let random = Random.State.make [| seed |] in
      try
        run_with_exn begin fun () ->
          let rec aux = function
            | 0 -> return ()
            | i ->
                let common = Random.State.int random 1000 in
                (* Note: need to reapply functor to get a fresh memory store *)
                let module Store = Irmin.Basic(Irmin_mem.Make)(Irmin.Contents.String) in
                let module T = Test_repo(Store) in
                let module Merge = Ck_merge.Make(T.Git)(T.Rev) in
                let open T in

                (* Printf.printf "test %d\n%!" i; *)
                let branch_d name =
                  Store.create config task >>= fun s ->
                  Store.remove_tag (s "git branch -d") name in
                Git.make config task >>= fun repo ->
                let commit ~parents msg =
                  branch_d msg >>= fun () ->
                  random_state ~common ~random repo >>= fun s ->
                  Git.Commit.commit ~parents s ~msg:[msg] >>= fun commit ->
                  Git.Repository.branch repo ~if_new:(lazy (return commit)) msg >>= fun _branch ->
                  return commit in
                commit ~parents:[] "base" >>= fun base ->
                commit ~parents:[base] "theirs" >>= fun theirs ->
                commit ~parents:[base] "ours" >>= fun ours ->

                let assert_git_tree_equals ~msg expected actual =
                  catch (fun () -> assert_git_tree_equals ~msg expected actual)
                    (fun ex ->
                      (* Commit the result so we can view it with git *)
                      branch_d "result" >>= fun () ->
                      Git.Repository.branch repo ~if_new:(lazy (return actual)) "result" >>= fun _ ->
                      fail ex) in

                (* This is the common case, and so is optimised *)
                Merge.merge ~base ~theirs:base ours >>= function
                | `Nothing_to_do -> assert false  (* (still need to update branch) *)
                | `Ok result ->
                assert (Git.Commit.equal result ours);

                (* This isn't, so we can use it to test the merge *)
                Merge.merge ~base ~theirs base >>= (function
                | `Nothing_to_do -> return theirs
                | `Ok result -> return result
                ) >>= assert_git_tree_equals ~msg:"theirs+base" theirs >>= fun () ->

                (* Add an extra commit on theirs to force it to try the trivial merge *)
                Git.Commit.checkout base >>= fun s ->
                Git.Commit.commit s ~msg:["empty commit"] >>= fun base2 ->
                Merge.merge ~base ~theirs:base2 ours >>= (function
                | `Nothing_to_do -> return base2
                | `Ok result -> return result
                ) >>= assert_git_tree_equals ~msg:"ours+base" ours >>= fun () ->

                Merge.merge ~base ~theirs ours >>= (function
                | `Nothing_to_do -> return base2
                | `Ok result -> return result
                ) >>= Rev.make ~time:(Ck_time.of_unix_time 0.0) >>= fun _rev ->
                aux (i - 1) in
          aux (try Sys.getenv "CK_TEST_ITERS" |> int_of_string with Not_found -> 100)
        end
      with ex ->
        Printf.printf "[ random seed = %d ]\n" seed;
        raise ex
    );
  ]

let is_error = function
  | RFailure _ | RError _ -> true
  | _ -> false

let () =
  Printexc.record_backtrace true;
  let results = run_test_tt_main suite in
  Format.print_newline ();
  if List.exists is_error results then exit 1
