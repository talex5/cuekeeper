open OUnit
open Lwt

(* let () = Log.(set_log_level INFO) *)

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
    let result, waker = Lwt.wait () in
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

module ItemMap = Map.Make(Key)
module Slow = Slow_set.Make(Test_clock)(Key)(ItemMap)
module Git = Git_storage.Make(Irmin.Basic(Irmin_mem.Make)(Irmin.Contents.String))
module M = Ck_model.Make(Test_clock)(Git)(struct type t = unit end)
module W = M.Widget

let format_list l = "[" ^ (String.concat "; " l) ^ "]"

type node = N of string * node list
let n name children = N (name, children)

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

let expect_tree s =
  match React.S.value s with
  | `Process rl | `Work rl -> rl
  | _ -> assert false

let expect_some = function
  | None -> assert false
  | Some x -> x

let assert_tree expected actual =
  let rec printer items = items
    |> List.map (fun (N (name, children)) -> name ^ "(" ^ printer children ^ ")")
    |> String.concat ", " in
  debug "Expecting: %s\n" (printer expected);
  assert_equal ~printer expected (get_tree actual)

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
        with Not_found -> Ck_utils.error "Node '%s' not found" p in
      match ps with
      | [] ->
          begin match W.item step with
          | `Item s -> React.S.value s
          | `Group label -> Ck_utils.error "Not an item '%s'" label end;
      | ps -> lookup ps (W.children step)

let run_with_exn fn =
  try
    fn () |> Lwt_main.run
  with ex ->
    Printexc.print_backtrace stderr;
    raise ex

let expect_action item =
  match item with
  | `Action x -> x
  | _ -> assert_failure "Not an action!"

let suite = 
  "cue-keeper">:::[
    "delay_rlist">:: (fun () ->
      Test_clock.time := 0.0;
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
        assert_equal ~printer:format_list expected actual in
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
    );

    "model">:: (fun () ->
      run_with_exn begin fun () ->
        Test_clock.time := 0.0;
        let config = Irmin_mem.config () in
        let task s =
          let date = Test_clock.now () |> Int64.of_float in
          Irmin.Task.create ~date ~owner:"User" s in
        let repo = Git.make config task in
        M.make repo >>= fun m ->
        M.set_mode m `Process;
        let process_tree = M.tree m |> expect_tree in
        let work = lookup ["Work"] process_tree in

        M.set_mode m `Work;
        let next_actions = M.tree m |> expect_tree in

        M.add_action ~parent:work ~name:"Write unit tests" ~description:"" m >>= fun _ ->

        (* Initially, we have a single Next action *)
        next_actions |> assert_tree [
          n "@Next actions" [
            n "@Start using CueKeeper" [
              n "@Read wikipedia page on GTD" []
            ];
            n "+Work" [
              n "@Write unit tests" []
            ]
          ];
          n "@Recently completed" []
        ];

        let read = lookup ["Next actions"; "Start using CueKeeper"; "Read wikipedia page on GTD"] next_actions in
        let units = lookup ["Next actions"; "Work"; "Write unit tests"] next_actions |> expect_action in

        (* After changing it to Waiting, it disappears from the list. *)
        M.set_action_state m units `Waiting >>= fun () ->
        M.delete m read >>= function
        | `Error x -> failwith x
        | `Ok () ->
        next_actions |> assert_tree [
          n "@Next actions" [
            n "-Start using CueKeeper" [
              n "@Read wikipedia page on GTD" []
            ];
            n "-Work" [
              n "@Write unit tests" []
            ]
          ];
          n "@Recently completed" [];
        ];
        Test_clock.run_to 2.0;
        next_actions |> assert_tree [
          n "Next actions" [];
          n "Recently completed" [];
        ];

        M.add_action ~parent:work ~name:"GC unused signals" ~description:"" m >>= fun _ ->
        next_actions |> assert_tree [
          n "Next actions" [
            n "+Work" [
              n "@GC unused signals" [];
            ]
          ];
          n "Recently completed" [];
        ];

        (* Get the updated units. *)
        let live_units = M.details m (`Action units) in
        let units = React.S.value (live_units.M.details_item) |> expect_some |> expect_action in
        assert (M.Item.action_state units <> `Next);
        (* Changing back to Next makes it reappear *)
        M.set_action_state m units `Next >>= fun () ->
        next_actions |> assert_tree [
          n "Next actions" [
            n "+Work" [
              n "@GC unused signals" [];
              n "+Write unit tests" []
            ];
          ];
          n "Recently completed" [];
        ];

        let units = React.S.value (live_units.M.details_item) |> expect_some |> expect_action in
        M.set_action_state m units `Waiting >>= fun () ->
        Test_clock.run_to 4.0;
        next_actions |> assert_tree [
          n "Next actions" [
            n "Work" [
              n "GC unused signals" [];
            ]
          ];
          n "Recently completed" [];
        ];

(*
        (* Rename conflict (e.g. two edits in different tabs *)
        let units = React.S.value (live_units.M.details_item) |> expect_some |> expect_action in
        M.set_name m units "Test conflicts" >>= fun () ->
        M.set_name m units "Fix merging" >>= fun () ->
*)

        return ()
      end
    )
  ]

let is_error = function
  | RFailure _ | RError _ -> true
  | _ -> false

let () =
  Printexc.record_backtrace true;
  let results = run_test_tt_main suite in
  Format.print_newline ();
  if List.exists is_error results then exit 1
