open Car
open TrafficLight

type lane_light_pair = {
  lane : Lane.t;
  light : TrafficLight.t;
}

type t
(** [t] is a traffic intersection composed of multiple lanes that tracks the
    pasage of time via step increments. *)

val set_rate : float -> int -> t -> t
(** [set_rate f i t] is a intersection with the the rate of the lane at index
    [i] in intersection [t] set to [f]. *)

val create : Car.t list array -> float array -> t
(** [create c r] creates an intersection where each of its lanes have its rate
    set to the corresponding float in [r] and start off with each car in each
    sublist in [c] being pushed to its corresponding lane. *)

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

val cars_in_intersection : t -> Car.t option array
(** [cars_in_intersection t] is the array of cars in the intersection [t] whose
    index in the array corresponds to their physical position in the
    intersection. *)

val string_of_intersection : t -> string
(** [string_of t] is the string representation of intersection [t]. *)

val list_lane_lights : t -> lane_light_pair list
(** [list_lane_lights t] is the list of the all pairs of lights and lanes in the
    intersection in a consistent order. *)

val get_lane_pair : t -> int -> lane_light_pair
(** [get_lane_pair t i] is the lane and light pair at index [i] in intersection
    [t]. The index corresponds to the physical position of the lane in the
    intersection. *)
