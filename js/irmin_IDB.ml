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
    IndexedDB_lwt.keys idb_store >>=
    Lwt_list.iter_p (fun x -> fn (K.of_hum x))
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

  type t = {
    r : R.t;
    watch : W.t;
    prefix : string;
    notifications : Html_storage.t;
    listener : Dom.event_listener_id Lazy.t;
  }

  let create config task =
    let db_name = Irmin.Private.Conf.get config db_name_key in
    let prefix = db_name ^ ".rw." in
    let watch = W.create () in
    let notifications = Html_storage.make () in
    (* Listens for changes made by other tabs.
     * Ideally there should be a way to stop listening, but for now we just make it lazy and only
     * use it in one place. *)
    let listener = lazy (
      Html_storage.watch notifications ~prefix (fun key value ->
        let subkey = tail key (String.length prefix) in
        let ir_key = K.of_hum subkey in
        let value = value >|?= Tc.read_string (module V) in
        W.notify watch ir_key value
      )
    ) in
    connect db_name >>= fun idb ->
    R.make (IndexedDB_lwt.store idb rw) task >|= fun make_r ->
    fun task -> { watch; r = make_r task; prefix; notifications; listener }

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
    |> IndexedDB_lwt.set t.r.idb_store (K.to_hum k) >|= fun () ->
    notify t k (Some value)

  let remove t k =
    (* Log.warn "Non-atomic remove called!"; *)
    IndexedDB_lwt.remove t.r.idb_store (K.to_hum k) >|= fun () ->
    notify t k None

  let compare_and_set t k ~test ~set =
    let pred old =
      match old, test with
      | None, None -> true
      | Some old, Some expected -> Tc.read_string (module V) old |> V.equal expected
      | _ -> false in
    let new_value = set >|?= Tc.write_string (module V) in
    IndexedDB_lwt.compare_and_set t.r.idb_store (K.to_hum k) ~test:pred ~new_value >|= function
    | true -> notify t k set; true
    | false -> false

  let watch t key =
    ignore (Lazy.force (t.listener));
    Irmin.Private.Watch.lwt_stream_lift (
      read t.r key >|= W.watch t.watch key
    )

  let watch_all t =
    ignore (Lazy.force (t.listener));
    W.watch_all t.watch

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
