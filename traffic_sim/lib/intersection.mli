open Car
open TrafficLight

type lane_light_pair = {
  lane : Lane.t;
  light : TrafficLight.t;
}
(** A value of type [lane_light_pair] represents a lane and its corresponding
    traffic light. *)

type t
(** [t] is a traffic intersection composed of multiple lanes that tracks the
    pasage of time via step increments. *)

val set_rate : float -> int -> t -> t
(** [set_rate f i t] is the intersection [t] with the the rate of the lane at
    index [i] in [t] set to [f]. NESW order. *)

val get_rate : int -> t -> float
(** [get_rate i f] is the rate of the lane at index [f] in intersection [t]. *)

val create : Car.t list array -> float array -> t
(** [create c r] creates an intersection where each of its lanes have its rate
    set to the corresponding float in [r] and start off with each car in each
    sublist in [c] being pushed to its corresponding lane. NESW order. *)

val empty : unit -> t
(** [empty] is the intial state of an intersection with empty lanes that do not
    spawn cars. *)

val get_steps : t -> int
(** [get_steps t] is the number of steps that have elapsed in intersection [t].
*)

val step : Car.t list array -> t -> t * Car.t option array
(** [step t arr] is the resulting intersection after [t] increments one time
    step. Each car in each sublist in [arr] is pushed on the corresponding lane
    in NESW order. It also returns an array of the cars that exited the
    intersection in (NW, NE, SE, SW order).
    - Raises: [Invalid_argument] if [Array.length arr] does not match the number
      of lanes.*)

val random_step : t -> t * Car.t option array
(** [random_step t] is the resulting intersection after [t] increments one time
    step and the array of cars that exited the intersection in (NW, NE, SE, SW
    order). Random cars are added to the end of each lane depending on the rate
    of each lane in [t]. *)

val cars_in_intersection : t -> Car.t option array
(** [cars_in_intersection t] is the array of cars in the intersection [t] whose
    index in the array corresponds to their physical position in the
    intersection (NW, NE, SE, SW order) *)

val list_lane_lights : t -> lane_light_pair list
(** [list_lane_lights t] is the list of the all pairs of lights and lanes in the
    intersection in NESW order. *)

val get_lane_pair : t -> int -> lane_light_pair
(** [get_lane_pair t i] is the lane-light pair at lane at index [i] (NESW order)
    in intersection [t]. *)

val add_one_car : t -> int -> Car.t -> t
(** [add_one_car i l c] is the intersection [i] with the car [c] added to the
    lane at index [l]. NESW order. *)

val get_num_cars : t -> int
(** [get_num_cars] returns the number of cars that currently in intersection
    [t]. *)
