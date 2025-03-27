open Car

type t
(** [t] is a lane with a rate of traffic flow, where cars of type [Car.t] enter,
    wait, and leave in FIFO (first-in, first-out) order. *)

exception No_car
(** Raised when an operation that accesses some car in a lane is attempted on an
    empty lane. *)

exception Invalid_rate
(** Raised when an the rate is set to a negative number. *)

val empty_lane : t
(** A lane without any cars. *)

val peek_car : t -> Car.t option
(** [peek_car t] is [Some c], where [c] is the car at the front of lane [t], if
    one is present. Returns [None] if the lane is empty. *)

val push_car : Car.t -> t -> unit
(** [push_car t c] adds car [c] to the back of lane [t]. *)

val pop_car : t -> Car.t
(** [pop_car t ] removes and returns the first car in lane [t].
    - Raises: [No_car] if lane [t] is empty. *)

val get_rate : t -> float
(** [get_rate t] is the rate of traffic flow in lane [t]. *)

val change_rate : t -> float -> t
(** [change_rate t r] is a copy of lane [t] with a rate of traffic flow of [r].
    - Raises: [Invalid_rate] if [r < 0.]. *)
