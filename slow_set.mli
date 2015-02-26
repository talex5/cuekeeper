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

module type ITEM = sig
  include Map.OrderedType
  val id : t -> Ck_id.t
end

module Make (C : Ck_clock.S) (I : ITEM) (M : Map.S with type key = I.t) : sig
  val make :
    delay : float ->
    eq : ('a -> 'a -> bool) ->
    ?init : 'a M.t ->
    'a M.t React.S.t ->
    ('a, 'b) item M.t React.S.t
end
