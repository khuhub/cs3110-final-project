open Car

type t
(** [t] is a traffic intersection composed of multiple lanes that tracks the
    pasage of time via step increments. *)

val get_steps : t -> int
(** [get_steps t] is the number of steps that have elapsed in intersection [t].
*)

val step : t -> Car.t list list -> t
(** [step t lst] is the resulting intersection after [t] increments one time
    step. Each car in each sublist in [lst] is pushed on the corresponding lane.
    - Raises: [Invalid_argument] if [List.length lst] does not match the number
      of lanes. *)

val random_step : t -> t
(** [random_step t] is the resulting intersection after [t] increments one time
    step. Random cars are added to the end of each lane. *)

val string_of_intersection : t -> string
(** [string_of_intersection t] is the string representation of intersection [t].
*)
