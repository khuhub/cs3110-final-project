type t
(**A city is a collection of intersections*)

val set_rate : float -> int -> int -> t -> t
(** [set_rate f i j t] is a city with the rate of the intersection at index
    [i][j] in intersection [t] set to [f]. *)

val get_rate : int -> int -> t -> float array
(** [get_rate i j t] is the array of rates for each lane of the intersection at
    index [i][j]. *)

val create : int -> int -> float -> t
(** [create rows cols rate] creates a city with the given number of row and
    columns, and the given rate on the four corners of the intersection.*)

val step : t -> t
(** [step t] is the city [t] after one time step: each intersection in [t] has
    incremented its time step and each car in each lane has moved forward one
    space. *)

val get_steps : t -> int
(** [get_steps t] is the number of steps that have elapsed in city [t]. *)

val get_intersections : t -> Intersection.t list list
(** [get_intersections t] is the matrix (immutable) of all intersections in the
    city.*)

val get_dimensions : t -> int * int
(** [get_dimensions t] is the width and height of the city *)

val num_cars : t -> int
(** [num_cars t] is the number of cars in city [t]. *)
