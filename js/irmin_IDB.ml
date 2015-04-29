(* Copyright (C) 2015, Thomas Leonard.
 * See the README file for details. *)

open Lwt
open Ck_utils

let err_not_found n =
  Lwt.fail (Invalid_argument (Printf.sprintf "IndexedDB.%s: not found" n))

let db_name_key =
  Irmin.Private.Conf.(key "indexedDB.db_name" string "CueKeeper")

let ao = IndexedDB_lwt.store_name "ao"
let rw = IndexedDB_lwt.store_name "rw"

let connect db_name =
  IndexedDB_lwt.make db_name ~init:(fun upgrader ->
    IndexedDB_lwt.create_store upgrader ao;
    IndexedDB_lwt.create_store upgrader rw;
  )

module RO (K: Irmin.Hum.S) (V: Tc.S0) = struct
  type key = K.t
  type value = V.t

  type t = {
    idb_store : IndexedDB_lwt.store;
    task : Irmin.task;
  }

  let task t = t.task

  let make idb_store task =
    return (fun a -> { task = task a; idb_store })

  let read t k =
    IndexedDB_lwt.get t.idb_store (K.to_hum k) >|= function
    | None -> None
    | Some s -> Some (Tc.read_string (module V) s)

  let read_exn t key =
    read t key >>= function
    | Some v -> return v
    | None -> err_not_found "read"

  let mem t k =
    IndexedDB_lwt.get t.idb_store (K.to_hum k) >|= function
    | None -> false
    | Some _ -> true

  let iter { idb_store; _ } fn =
    IndexedDB_lwt.bindings idb_store >>=
    Lwt_list.iter_p (fun (k, v) -> fn (K.of_hum k) (return (Tc.read_string (module V) v)))
end

module AO (K: Irmin.Hash.S) (V: Tc.S0) = struct
  include RO(K)(V)

  let create config task =
    let db_name = Irmin.Private.Conf.get config db_name_key in
    connect db_name >>= fun idb ->
    make (IndexedDB_lwt.store idb ao) task

  let add t value =
    let k = Tc.write_cstruct (module V) value |> K.digest in
    let v = Tc.write_string (module V) value in
    IndexedDB_lwt.set t.idb_store (K.to_hum k) v >|= fun () -> k
end

module RW (K: Irmin.Hum.S) (V: Tc.S0) = struct
  module W = Irmin.Private.Watch.Make(K)(V)
  module R = RO(K)(V)
  open R

  type watch = W.watch

  type t = {
    r : R.t;
    watch : W.t;
    prefix : string;
    notifications : Html_storage.t;
    mutable listener : (Dom.event_listener_id * int) option;
  }

  let create config task =
    let db_name = Irmin.Private.Conf.get config db_name_key in
    let prefix = db_name ^ ".rw." in
    let watch = W.create () in
    let notifications = Html_storage.make () in
    connect db_name >>= fun idb ->
    R.make (IndexedDB_lwt.store idb rw) task >|= fun make_r ->
    fun task -> { watch; r = make_r task; prefix; notifications; listener = None }

  let ref_listener t =
    match t.listener with
    | None ->
        let l =
          Html_storage.watch t.notifications ~prefix:t.prefix (fun key value ->
            let subkey = tail key (String.length t.prefix) in
            let ir_key = K.of_hum subkey in
            let value = value >|?= Tc.read_string (module V) in
            async (fun () -> W.notify t.watch ir_key value)
          ) in
        t.listener <- Some (l, 1)
    | Some (l, n) ->
        t.listener <- Some (l, n + 1)

  let unref_listener t =
    match t.listener with
    | None -> failwith "unref_listener, but not listening!"
    | Some (l, 1) ->
        Dom.removeEventListener l;
        t.listener <- None
    | Some (l, n) ->
        assert (n > 1);
        t.listener <- Some (l, n - 1)

  let notify t k new_value =
    (* Notify other tabs *)
    begin match new_value with
    | None -> Html_storage.remove t.notifications (t.prefix ^ K.to_hum k)
    | Some v -> Html_storage.set t.notifications (t.prefix ^ K.to_hum k) (Tc.write_string (module V) v)
    end;
    (* Notify this tab *)
    W.notify t.watch k new_value

  let update t k value =
    (* Log.warn "Non-atomic update called!"; *)
    Tc.write_string (module V) value
    |> IndexedDB_lwt.set t.r.idb_store (K.to_hum k) >>= fun () ->
    notify t k (Some value)

  let remove t k =
    (* Log.warn "Non-atomic remove called!"; *)
    IndexedDB_lwt.remove t.r.idb_store (K.to_hum k) >>= fun () ->
    notify t k None

  let compare_and_set t k ~test ~set =
    let pred old =
      match old, test with
      | None, None -> true
      | Some old, Some expected -> Tc.read_string (module V) old |> V.equal expected
      | _ -> false in
    let new_value = set >|?= Tc.write_string (module V) in
    IndexedDB_lwt.compare_and_set t.r.idb_store (K.to_hum k) ~test:pred ~new_value >>= function
    | true -> notify t k set >|= fun () -> true
    | false -> return false

  let watch t ?init cb =
    ref_listener t;
    W.watch t.watch ?init cb

  let unwatch t w =
    unref_listener t;
    W.unwatch t.watch w

  let watch_key t key ?init cb =
    ref_listener t;
    W.watch_key t.watch key ?init cb

  let iter t = R.iter t.r
  let mem t = R.mem t.r
  let read t = R.read t.r
  let read_exn t = R.read_exn t.r
  let task t = R.task t.r
  type value = R.value
  type key = R.key
end

let config db_name = Irmin.Private.Conf.singleton db_name_key db_name

module Make (C: Irmin.Contents.S) (T: Irmin.Tag.S) (H: Irmin.Hash.S) =
  Irmin.Make(AO)(RW)(C)(T)(H)
