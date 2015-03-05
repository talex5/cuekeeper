(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

type state =
  [ `New
  | `Current
  | `Removed of float ] (* Time item was removed from input *)

type 'a item

val data : 'a item -> 'a
val state : 'a item -> state React.S.t

module type SORT_KEY = sig
  include Map.OrderedType
  module Id : Map.OrderedType
  val id : t -> Id.t
  val show : t -> string (* For debugging *)
end

module Make (C : Ck_clock.S) (K : SORT_KEY) (M : Map.S with type key = K.t) : sig
  val make :
    delay : float ->
    eq : ('a -> 'a -> bool) ->
    ?init : 'a M.t ->
    'a M.t React.S.t ->
    'a item M.t React.S.t
end
