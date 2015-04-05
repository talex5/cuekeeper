(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

type time_unit =
  | Day
  | Week
  | Month
  | Year
  with sexp

(** A date as entered by the user, without timezone information.
 * 1st Jan 2015 is (2015, 0, 1) - months start at zero. *)
type user_date = private (int * int * int) with sexp

type repeat = private {
  repeat_n : int;
  repeat_unit : time_unit;
  repeat_from : user_date;
} with sexp

val make_repeat : from:user_date -> int -> time_unit -> repeat

val of_tm : Unix.tm -> user_date
val tm_of : user_date -> Unix.tm
val of_unix_time : float -> user_date
val unix_time_of : user_date -> float

(** Create a [user_date].
 * Note: months start from 0, not 1.
 * Values are normalised so that, e.g., 40 October is changed into 9 November. *)
val make : year:int -> month:int -> day:int -> user_date

val compare : user_date -> user_date -> int

(** [next_repeat ~now r] gives the next time that [r.repeat_from] should be set to,
 * by moving [repeat_from] forward in steps of [repeat_n * repeat_unit] until it's
 * after [now]. *)
val next_repeat : now:user_date -> repeat -> user_date

val string_of_time_unit : time_unit -> string
val string_of_unix_time : float -> string
val string_of_user_date : user_date -> string
val string_of_repeat : repeat -> string
