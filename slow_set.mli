(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

type 'a state =
  [ `New
  | `Moved of 'a option ref    (* Ref is shared with removed item *)
  | `Current
  | `Removed of 'a option ref * float ] (* Time item was removed from input *)

type ('a, 'b) item

val data : ('a, 'b) item -> 'a
val state : ('a, 'b) item -> 'b state React.S.t

module type SORT_KEY = sig
  include Map.OrderedType
  val id : t -> Ck_id.t
  val show : t -> string (* For debugging *)
end

module Make (C : Ck_clock.S) (K : SORT_KEY) (M : Map.S with type key = K.t) : sig
  val make :
    delay : float ->
    eq : ('a -> 'a -> bool) ->
    ?init : 'a M.t ->
    'a M.t React.S.t ->
    ('a, 'b) item M.t React.S.t
end
