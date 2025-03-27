open Car

type t
(** [t] is a traffic intersection composed of multiple lanes that tracks the
    pasage of time via step increments. *)
val empty_intersection : t

val create : Car.t list list -> t
(** [create lst] creates an intersection with each list of cars in [lst] being
    added to its corresponding lane. *)

val empty : t
(** [empty] is the intial state of the world. All lanes are empty, and traffic
    lights in the north and south directions are green and those in the east and
    west directions are red. *)

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

val list_of_lane_lights : t -> (Lane.t * TrafficLight.TrafficLight.t) list
(** [list_lane_light_pairs t] is the list of the all pairs of lights and lanes
    in the intersection in a consistent order. *)
