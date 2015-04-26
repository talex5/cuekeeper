(* Copyright (C) 2015, Thomas Leonard.
 * See the README file for details. *)

open Lwt
open Ck_utils

type db = IndexedDB.database Js.t
type store_name = Js.js_string Js.t

type store = {
  db : db;
  store_name : store_name;

  (* We reuse transactions where possible for performance.
   * This does mean that if any read fails then the others will hang, but we treat any
   * read failing as a fatal error anyway. *)
  mutable ro_trans : IndexedDB.transaction Js.t option;
}

type key = string
type db_name = string
type db_upgrader = IndexedDB.database Js.t
let store_name = Js.string

let make db_name ~init =
  let factory : IndexedDB.factory Js.t Js.Optdef.t = (Obj.magic Dom_html.window)##indexedDB in
  Js.Optdef.case factory
    (fun () -> failwith "IndexedDB not available")
    (fun factory ->
      let request = factory##_open (Js.string db_name, 2) in
      request##onupgradeneeded <- Dom.handler (fun _event ->
        init (request##result);
        Js._true
      );
      let t, set_t = Lwt.wait () in
      request##onerror <- Dom.handler (fun _event ->
        Lwt.wakeup_exn set_t (Failure "Error trying to connect to IndexedDB!");
        Js._true
      );
      request##onsuccess <- Dom.handler (fun _event ->
        Lwt.wakeup set_t (request##result);
        Js._true
      );
      t
    )

let store db store_name = { db; store_name; ro_trans = None }

let create_store db name =
  db##createObjectStore (name) |> ignore

let opt_string x ~if_missing =
  Js.Optdef.case x
    (fun () -> if_missing)
    (fun x -> Js.to_string x)

let idb_error typ (event:IndexedDB.request IndexedDB.errorEvent Js.t) =
  let msg =
    Js.Opt.case (event##target)
      (fun () -> "(missing target on error event)")
      (fun target ->
        Js.Opt.case (target##error)
          (fun () -> "(missing error on request)")
          (fun error ->
            let name = opt_string (error##name) ~if_missing:"(no name)" in
            let message = opt_string (error##message) ~if_missing:"(no message)" in
            let code = Js.Optdef.get (error##code) (fun () -> 0) in
            Printf.sprintf "%s: %s (error code %d)" name message code
          )
      ) in
  Failure (Printf.sprintf "IndexedDB transaction (%s) failed: %s" typ msg)

let rec trans_ro (t:store) setup =
  let r, set_r = Lwt.wait () in
  match t.ro_trans with
  | None ->
      let trans = t.db##transaction (Js.array [| t.store_name |], Js.string "readonly") in
      t.ro_trans <- Some trans;
      trans##onerror <- Dom.handler (fun event ->
        t.ro_trans <- None;
        (* Fatal error *)
        !Lwt.async_exception_hook (idb_error "RO" event);
        Js._true
      );
      trans##oncomplete <- Dom.handler (fun _event ->
        t.ro_trans <- None;
        Js._true
      );
      setup (trans##objectStore (t.store_name)) set_r;
      r
  | Some trans ->
      (* Seems we can get here when a transaction is done but oncomplete hasn't been called,
       * so retry if we get an error. *)
      try setup (trans##objectStore (t.store_name)) set_r; r
      with _ex ->
        t.ro_trans <- None;
        trans_ro t setup

let trans_rw t setup =
  let r, set_r = Lwt.wait () in
  let trans = t.db##transaction (Js.array [| t.store_name |], Js.string "readwrite") in
  trans##onerror <- Dom.handler (fun event ->
    Lwt.wakeup_exn set_r (idb_error "RW" event);
    Js._true
  );
  trans##oncomplete <- Dom.handler (fun _event ->
    Lwt.wakeup set_r ();
    Js._true
  );
  setup (trans##objectStore (t.store_name));
  r

let bindings t =
  let bindings = ref [] in
  trans_ro t
    (fun store set_r ->
      let request = store##openCursor () in
      request##onsuccess <- Dom.handler (fun _event ->
        Js.Opt.case (request##result)
          (fun () -> Lwt.wakeup set_r !bindings)
          (fun cursor ->
            let key = cursor##key |> Js.to_string in
            let value = cursor##value |> Js.to_string |> UTF8_codec.decode in
            bindings := (key, value) :: !bindings;
            cursor##continue ()
          );
        Js._true
      )
    )

let set t key value =
  trans_rw t (fun store ->
    store##put (Js.string (UTF8_codec.encode value), Js.string key) |> ignore
  )

let remove t key =
  trans_rw t
    (fun store ->
      store##delete (Js.string key) |> ignore
    )

let get t key =
  trans_ro t
    (fun store set_r ->
      let request = store##get (Js.string key) in
      request##onsuccess <- Dom.handler (fun _event ->
        Js.Optdef.case (request##result)
          (fun () -> None)
          (fun s -> Some (Js.to_string s |> UTF8_codec.decode))
        |> Lwt.wakeup set_r;
        Js._true
      )
    )

let compare_and_set t key ~test ~new_value =
  let result = ref None in
  let key = Js.string key in
  trans_rw t
    (fun store ->
      let request = store##get (key) in
      request##onsuccess <- Dom.handler (fun _event ->
        let actual =
          Js.Optdef.to_option (request##result)
          >|?= fun x -> Js.to_string x |> UTF8_codec.decode in
        if test actual then (
          begin match new_value with
          | None -> store##delete (key) |> ignore
          | Some new_value -> store##put (Js.string (UTF8_codec.encode new_value), key) |> ignore end;
          result := Some true
        ) else (
          result := Some false
        );
        Js._true
      )
    )
  >|= fun () ->
  match !result with
  | None -> failwith "Transaction completed, but no result!"
  | Some x -> x
