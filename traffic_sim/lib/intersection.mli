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
    [i] in intersection [t] set to [f]. NESW order. *)

val get_rate : int -> t -> float
(** [get_rate i f] is the rate of the lane at index [f]. *)

val create : Car.t list array -> float array -> t
(** [create c r] creates an intersection where each of its lanes have its rate
    set to the corresponding float in [r] and start off with each car in each
    sublist in [c] being pushed to its corresponding lane. NESW order. *)

val empty : unit -> t
(** [empty] is the intial state of the world. All lanes are empty, and traffic
    lights in the north and south directions are green and those in the east and
    west directions are red. *)

val get_steps : t -> int
(** [get_steps t] is the number of steps that have elapsed in intersection [t].
*)

val step : Car.t list array -> t -> t * Car.t option array
(** [step t arr] is the resulting intersection after [t] increments one time
    step. Each car in each sublist in [arr] is pushed on the corresponding lane.
    It also returns an array of the cars left the intersection during this step,
    with their index in the array corresponding to the index of the intersection
    they were last at.
    - Raises: [Invalid_argument] if [Array.length arr] does not match the number
      of lanes. NESW order *)

val random_step : t -> t * Car.t option array
(** [random_step t] is the resulting intersection after [t] increments one time
    step. Random cars are added to the end of each lane. *)

val cars_in_intersection : t -> Car.t option array
(** [cars_in_intersection t] is the array of cars in the intersection [t] whose
    index in the array corresponds to their physical position in the
    intersection (NW, NE, SE, SW order) *)

val list_lane_lights : t -> lane_light_pair list
(** [list_lane_lights t] is the list of the all pairs of lights and lanes in the
    intersection in NESW order. *)

val get_lane_pair : t -> int -> lane_light_pair
(** [get_lane_pair t i] is the lane and light pair at index [i] in intersection
    [t]. The index corresponds to the physical position of the lane in the
    intersection. *)

val add_one_car : t -> int -> Car.t -> t
(** [add_one_car i l c] is the intersection [i] with the car [c] added to the
    lane at index [l]. The index corresponds to the physical position of the
    lane in the intersection. *)

val get_num_cars : t -> int
