(* Copyright (C) 2015, Thomas Leonard.
 * See the README file for details. *)

(** js_of_ocaml type declarations for the w3c IndexedDB spec: http://www.w3.org/TR/IndexedDB/
 * Currently only covers the bits needed for CueKeeper.
 * IndexedDB_lwt provides a more friendly API. *)

(* Note: we currently assume all keys and values are strings.
 * This will always be the case for entries added using this interface. *)
type key = Js.js_string Js.t
type value = Js.js_string Js.t
type store_name = Js.js_string Js.t
type mode = Js.js_string Js.t

class type upgradeneeded = object
  inherit Dom_html.event
end
class type ['a] errorEvent = object
  inherit ['a] Dom.event
end
class type completeEvent = object
  inherit Dom_html.event
end
class type successEvent = object
  inherit Dom_html.event
end

class type cursor = object
  method key : key Js.readonly_prop
  method continue : unit Js.meth
end
class type cursorWithValue = object
  inherit cursor
  method value : value Js.readonly_prop
end

class type dom_exception = object
  (* Being a bit paranoid marking all these as optdef *)
  method name : Js.js_string Js.t Js.Optdef.t Js.readonly_prop
  method message : Js.js_string Js.t Js.Optdef.t Js.readonly_prop
  method code : int Js.Optdef.t Js.readonly_prop
end

class type request = object
  method error : dom_exception Js.t Js.Opt.t Js.readonly_prop
  method onerror : ('self Js.t, request errorEvent Js.t) Dom.event_listener Js.prop
  method onsuccess : ('self Js.t, successEvent Js.t) Dom.event_listener Js.prop
end

class type getRequest = object ('self)
  inherit request
  method result : value Js.Optdef.t Js.readonly_prop
end

class type openCursorRequest = object
  inherit request
  method result : cursorWithValue Js.t Js.Opt.t Js.readonly_prop
end

class type objectStore = object
  method add : value -> key -> request Js.t Js.meth
  method put : value -> key -> request Js.t Js.meth
  method delete : key -> request Js.t Js.meth
  method get : key -> getRequest Js.t Js.meth
  method openCursor : openCursorRequest Js.t Js.meth
end

class type transaction = object
  method oncomplete : ('self Js.t, completeEvent Js.t) Dom.event_listener Js.prop
  method onerror : ('self Js.t, request errorEvent Js.t) Dom.event_listener Js.prop
  method objectStore : store_name -> objectStore Js.t Js.meth
  method abort : unit Js.meth
end

class type database = object
  method close : unit Js.meth
  method createObjectStore : store_name -> objectStore Js.t Js.meth
  method onerror : ('self Js.t, request errorEvent Js.t) Dom.event_listener Js.prop
  method transaction : store_name Js.js_array Js.t -> mode -> transaction Js.t Js.meth
end

class type openDBRequest = object ('self)
  inherit request
  method onupgradeneeded : ('self Js.t, upgradeneeded Js.t) Dom_html.event_listener Js.prop
  method result : database Js.t Js.readonly_prop
end

class type factory = object
  method _open : Js.js_string Js.t -> int -> openDBRequest Js.t Js.meth
end
