(* Copyright (C) 2015, Thomas Leonard.
 * See the README file for details. *)

open Lwt

module Log = Log.Make(struct let section = "HTML_ST" end)

let err_not_found n =
  Lwt.fail (Invalid_argument (Printf.sprintf "Html_storage.%s: not found" n))

let prefix_key =
  Irmin.Private.Conf.(key "html5storage.prefix" string "CueKeeper")

module Storage : sig
  type t
  type key = string

  val make : unit -> t
  val get : t -> key -> string option
  val set : t -> key -> string -> unit
  val remove : t -> key -> unit
  val keys : t -> prefix:string -> key list
end = struct
  type t = Dom_html.storage Js.t
  type key = string

  let make () =
    Js.Optdef.get (Dom_html.window##localStorage)
      (fun () -> failwith "HTML 5 storage is not available")

  let get t key =
    Js.Opt.case (t##getItem (Js.string key))
      (fun () -> None)
      (fun v -> Some (Js.to_string v |> B64.decode))

  let set t key value =
    let encoded = B64.encode value |> Js.string in
    t##setItem (Js.string key, encoded)

  let remove t key =
    t##removeItem (Js.string key)

  let tail s i =
    String.sub s i (String.length s - i)

  let keys t ~prefix =
    let prefix_len = String.length prefix in
    let key_at i =
      Js.Opt.case (t##key (i))
        (fun () -> failwith "Key disappeared during iteration!")
        Js.to_string in
    (* Take a lock here? *)
    let rec aux acc = function
      | 0 -> acc
      | i ->
          let k = key_at (i - 1) in
          let acc =
            if String.length k > prefix_len && String.sub k 0 prefix_len = prefix then
              tail k prefix_len :: acc
            else acc in
          aux acc (i - 1)  in
    aux [] t##length
end

module RO (K: Irmin.Hum.S) (V: Tc.S0) = struct

  module W = Irmin.Private.Watch.Make(K)(V)

  type key = K.t

  type value = V.t

  type t = {
    prefix : string;
    w : W.t;
    task : Irmin.task;
    s: Storage.t;
  }

  let task t = t.task

  let make s prefix =
    let w = W.create () in
    fun task ->
      return (fun a -> { w; task = task a; s; prefix })

  let js_key t k =
    t.prefix ^ K.to_hum k

  let read_raw t k =
    match Storage.get t.s (js_key t k) with
    | None -> None
    | Some s -> Some (Tc.read_string (module V) s)

  let read t k = return (read_raw t k)

  let read_exn t key =
    match read_raw t key with
    | Some v -> return v
    | None -> err_not_found "read"

  let mem t k =
    return (Storage.get t.s (js_key t k) <> None)

  let iter { s; prefix; _ } fn =
    Storage.keys s ~prefix
    |> Lwt_list.iter_p (fun x -> fn (K.of_hum x))
end

module AO (K: Irmin.Hash.S) (V: Tc.S0) = struct

  include RO(K)(V)

  let create config task =
    let prefix = Irmin.Private.Conf.get config prefix_key in
    let s = Storage.make () in
    make s (prefix ^ ".ao.") task

  let add t value =
    let k = Tc.write_cstruct (module V) value |> K.digest in
    let v = Tc.write_string (module V) value in
    Storage.set t.s (js_key t k) v;
    return k
end

module RW (K: Irmin.Hum.S) (V: Tc.S0) = struct

  include RO(K)(V)

  let create config task =
    let prefix = Irmin.Private.Conf.get config prefix_key in
    let s = Storage.make () in
    make s (prefix ^ ".rw.") task

  let update t k value =
    Tc.write_string (module V) value
    |> Storage.set t.s (js_key t k);
    W.notify t.w k (Some value);
    return_unit

  let remove t k =
    Storage.remove t.s (js_key t k);
    W.notify t.w k None;
    return_unit

  let watch t key =
    (* TODO: watch changes made by other pages? *)
    Irmin.Private.Watch.lwt_stream_lift (
      read t key >|= W.watch t.w key
    )

  let watch_all t = W.watch_all t.w
end

let config prefix = Irmin.Private.Conf.singleton prefix_key prefix

module Make (C: Irmin.Contents.S) (T: Irmin.Tag.S) (H: Irmin.Hash.S) =
  Irmin.Make(AO)(RW)(C)(T)(H)
