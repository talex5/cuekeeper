(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

type state =
  [ `New
  | `Moved
  | `Current
  | `Removed of float ] (* Time item was removed from input *)

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
    ('a * state) M.t React.S.t
end
