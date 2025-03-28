open Car

type t
(** [t] is a traffic intersection composed of multiple lanes that tracks the
    pasage of time via step increments. *)

val create : Car.t list array -> t
(** [create arr] creates an intersection with each list of cars in [arr] being
    added to its corresponding lane. *)

val empty : t
(** [empty] is the intial state of the world. All lanes are empty, and traffic
    lights in the north and south directions are green and those in the east and
    west directions are red. *)

val get_steps : t -> int
(** [get_steps t] is the number of steps that have elapsed in intersection [t].
*)

val step : Car.t list array -> t -> t
(** [step t arr] is the resulting intersection after [t] increments one time
    step. Each car in each sublist in [arr] is pushed on the corresponding lane.
    - Raises: [Invalid_argument] if [Array.length arr] does not match the number
      of lanes. *)

val random_step : t -> t
(** [random_step t] is the resulting intersection after [t] increments one time
    step. Random cars are added to the end of each lane. *)

val string_of_intersection : t -> string
(** [string_of t] is the string representation of intersection [t]. *)

val list_lane_lights : t -> (Lane.t * TrafficLight.TrafficLight.t) list
(** [list_lane_lights t] is the list of the all pairs of lights and lanes in the
    intersection in a consistent order. *)
