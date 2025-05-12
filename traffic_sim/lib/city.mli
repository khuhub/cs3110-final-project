type t
(**A city is a collection of intersections*)

val set_rate : float -> int -> int -> t -> t
(** [set_rate rate i j t] is the city [t] with the rate of the intersection at
    index [i][j] in intersection [t] set to [rate]. *)

val get_rate : int -> int -> t -> float array
(** [get_rate i j t] is an array of rates in NESW order for each lane of the
    intersection at index [i][j]. *)

val create : int -> int -> float -> t
(** [create r c rate] creates a city with the [r] rows and [c] colums, and with
    the rate at the four corners of the city set to [rate].
    - Requires: rate is an decimal between [0.0] and [1.0], and [r] and [c] are
      positive integers. *)

val add_one_car : Car.Car.t -> int -> int -> int -> t -> t
(** [add_car_lst car i j l t] is the city [t] with the car [car] pushed on to
    the lane [l] of the intersection at index [i][j]. *)

val step : t -> t
(** [step t] is the city [t] after one time step. Each intersection in [t] has
    incremented its time step and each car in each lane has moved forward one
    space if it can. *)

val get_steps : t -> int
(** [get_steps t] is the number of steps that have elapsed in city [t]. *)

val get_intersections : t -> Intersection.t list list
(** [get_intersections t] is the matrix (immutable) of all intersections in the
    city. *)

val get_dimensions : t -> int * int
(** [get_dimensions t] is the width and height of the city [t]. *)

val num_cars : t -> int
(** [num_cars t] is the number of cars in the city [t]. *)
