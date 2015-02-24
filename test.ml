open OUnit
open Lwt

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

  let async f =
    let (_ : unit Lwt.t) =
      catch (fun () -> sleep 0.0 >>= f)
        (fun ex -> raise ex) in
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

module StringSet = Set.Make(String)
module Slow = Slow_set.Make(Test_clock)(String)(StringSet)

module Store = Irmin.Basic(Irmin_mem.Make)(Irmin.Contents.String)
module M = Ck_model.Make(Test_clock)(Store)

let format_list l = "[" ^ (String.concat "; " l) ^ "]"

let suite = 
  "cue-keeper">:::[
    "delay_rlist">:: (fun () ->
      Test_clock.time := 0.0;
      let src, set_src = React.S.create StringSet.empty in
      let set items =
        List.fold_left (fun acc i -> StringSet.add i acc) StringSet.empty items
        |> set_src in
      let eqd rl expected =
        let actual =
          Slow.M.fold (fun key state acc ->
            let s = match state with
              | `New -> "+" ^ key
              | `Current -> key
              | `Removed _ -> "-" ^ key in
            s :: acc
          ) (React.S.value rl) []
          |> List.rev in
        assert_equal ~printer:format_list expected actual in
      let dst = Slow.make ~delay:1.0 src in
      eqd dst [];

      set ["1"];
      eqd dst ["+1"];

      set [];
      eqd dst ["-1"];
      Test_clock.run_to 2.0;
      eqd dst [];

      set ["1"; "2"; "3"];
      set ["1"; "3"];   (* Remove 2 at t=2.0 *)
      eqd dst ["+1"; "-2"; "+3"];

      set ["0"; "1"; "1.5"; "3"];
      eqd dst ["+0"; "+1"; "+1.5"; "-2"; "+3"];

      set ["0"; "1"; "1.5"; "3"; "3.5"];
      eqd dst ["+0"; "+1"; "+1.5"; "-2"; "+3"; "+3.5"];
      Test_clock.run_to 2.1;
      eqd dst ["+0"; "+1"; "+1.5"; "-2"; "+3"; "+3.5"];

      set ["1"; "1.5"; "3"; "3.5"];
      eqd dst ["-0"; "+1"; "+1.5"; "-2"; "+3"; "+3.5"];
      Test_clock.run_to 3.0;
      eqd dst ["-0"; "1"; "1.5"; "3"; "3.5"];
      Test_clock.run_to 3.1;
      eqd dst ["1"; "1.5"; "3"; "3.5"];

      set ["1"; "1.5"; "3"];
      eqd dst ["1"; "1.5"; "3"; "-3.5"];
      Test_clock.run_to 4.1;
      eqd dst ["1"; "1.5"; "3"];

      set ["1.5"; "3"];
      eqd dst ["-1"; "1.5"; "3"];
      Test_clock.run_to 5.1;
      eqd dst ["1.5"; "3"];

      set ["0"; "3"];
      eqd dst ["+0"; "-1.5"; "3"];

      Test_clock.run_to 6.1;
      eqd dst ["0"; "3"];

      set ["4"];
      eqd dst ["-0"; "-3"; "+4"];
      Test_clock.run_to 7.1;
      eqd dst ["4"];
    );

    "model">:: (fun () ->
      Lwt_unix.run begin
        Test_clock.time := 0.0;
        let config = Irmin_mem.config () in
        let task s =
          let date = Test_clock.now () |> Int64.of_float in
          Irmin.Task.create ~date ~owner:"User" s in
        Store.create config task >>= M.make >>= fun m ->
        let root = M.uuid (React.S.value (M.root m)) in
        M.add_area ~parent:root ~name:"Personal" ~description:"" m >>= fun _personal ->
        M.add_area ~parent:root ~name:"Work" ~description:"" m >>= fun work ->
        M.add_action ~parent:work ~name:"Write unit tests" ~description:"" m >>= fun _units ->
        let next_actions = M.work_tree m in
        match ReactiveData.RList.value next_actions with
        | ([] | _::_::_) -> assert false
        | [units] ->
        assert (React.S.value units.M.View.name = "Write unit tests");
        M.set_state m units.M.View.uuid (`Action {Ck_sigs.astate = `Waiting}) >>= fun () ->
        assert (List.length (ReactiveData.RList.value next_actions) = 1);
        Test_clock.run_to 2.0;
        assert (List.length (ReactiveData.RList.value next_actions) = 0);
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
