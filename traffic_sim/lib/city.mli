type t
(**A city is a collection of intersections*)

val set_rate : float -> int -> int -> t -> t
(** [set_rate f i j t] is a city with the rate of the intersection at index
    [i][j] in intersection [t] set to [f]. *)

val create : int -> int -> float -> t

val step : t -> t
(** [step t] is the city [t] after one time step: each intersection in [t] has
    incremented its time step and each car in each lane has moved forward one
    space. *)

val get_steps : t -> int
(** [get_steps t] is the number of steps that have elapsed in city [t]. *)

val num_cars : t -> int
(** [num_cars t] is the number of cars in city [t]. *)

val string_of_city : t -> string
