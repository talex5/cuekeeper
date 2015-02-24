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

module D = Delay_RList.Make (Test_clock)

module Store = Irmin.Basic(Irmin_mem.Make)(Irmin.Contents.String)
module M = Ck_model.Make(Store)

let format_list l = "[" ^ (String.concat "; " l) ^ "]"

let suite = 
  "cue-keeper">:::[
    "delay_rlist">:: (fun () ->
      let open ReactiveData.RList in
      let eq rl expected =
        assert_equal ~printer:format_list expected (value rl) in
      let eqd rl expected =
        let actual = value rl |> List.map (fun d ->
          let s = d.Delay_RList.data in
          match React.S.value d.Delay_RList.state with
          | `New -> "+" ^ s
          | `Current -> s
          | `Removed -> "-" ^ s
        ) in
        assert_equal ~printer:format_list expected actual in
      let src, handle = make [] in
      let dst = D.make ~delay:1.0 src in
      eqd dst [];
      insert "first" 0 handle;
      eqd dst ["+first"];
      remove 0 handle;
      eqd dst ["-first"];
      Test_clock.run_to 2.0;
      eqd dst [];

      insert "first" 0 handle;
      insert "second" 1 handle;
      insert "third" 2 handle;
      remove 1 handle;            (* Remove second at t=2.0 *)
      eq src ["first"; "third"];
      eqd dst ["+first"; "-second"; "+third"];
      insert "zero" 0 handle;
      insert "1.5" 2 handle;
      eq src ["zero"; "first"; "1.5"; "third"];
      eqd dst ["+zero"; "+first"; "-second"; "+1.5"; "+third"];
      insert "3.5" (-1) handle;
      eq src ["zero"; "first"; "1.5"; "third"; "3.5"];
      eqd dst ["+zero"; "+first"; "-second"; "+1.5"; "+third"; "+3.5"];
      Test_clock.run_to 2.1;
      remove 0 handle;            (* Remove zero at t=2.1 *)
      eq src ["first"; "1.5"; "third"; "3.5"];
      eqd dst ["-zero"; "+first"; "-second"; "+1.5"; "+third"; "+3.5"];
      Test_clock.run_to 3.0;
      eqd dst ["-zero"; "first"; "1.5"; "third"; "3.5"];
      Test_clock.run_to 3.1;
      eqd dst ["first"; "1.5"; "third"; "3.5"];

      remove (-1) handle;
      eq src ["first"; "1.5"; "third"];
      eqd dst ["first"; "1.5"; "third"; "-3.5"];
      Test_clock.run_to 4.1;
      eqd dst ["first"; "1.5"; "third"];

      remove 0 handle;
      update "middle" 0 handle;
      eq src ["middle"; "third"];
      eqd dst ["-first"; "middle"; "third"];
      Test_clock.run_to 5.1;
      eqd dst ["middle"; "third"];

      remove 0 handle;
      insert "zero" 0 handle;
      move 0 1 handle;
      eq src ["third"; "zero"];
      eqd dst ["-middle"; "third"; "+zero"];

      move 1 (-1) handle;
      eq src ["zero"; "third"];
      eqd dst ["-middle"; "+zero"; "third"];
      Test_clock.run_to 6.1;
      eqd dst ["zero"; "third"];

      insert "end" (-1) handle;
      remove 0 handle;
      remove 0 handle;
      remove 0 handle;
      eqd dst ["-zero"; "-third"; "-end"];
      Test_clock.run_to 7.1;
      eqd dst [];
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
        assert (React.S.value units.M.name = "Write unit tests");
        M.set_state m units.M.uuid (`Action {Ck_sigs.astate = `Waiting}) >>= fun () ->
        assert (List.length (ReactiveData.RList.value next_actions) = 0);
        return ()
      end
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
