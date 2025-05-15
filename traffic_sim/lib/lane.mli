open Car

type t
(** [t] is a lane with a rate of traffic flow, where cars of type [Car.t] enter,
    wait, and leave in FIFO (first-in, first-out) order. *)

exception Invalid_rate
(** Raised when an the rate is set to a negative number. *)

val empty_lane : t
(** A lane without any cars. *)

val peek_car : t -> Car.t option
(** [peek_car t] is [Some c], where [c] is the car at the front of lane [t], if
    one is present. Returns [None] if the lane is empty. *)

val push_car : Car.t -> t -> t
(** [push_car t c] adds car [c] to the back of lane [t]. *)

val pop_car : t -> (Car.t * t) option
(** [pop_car t ] is tuple containing the car that was popped from [t] and the
    resulting lane, or [None] if [t] is empty. *)

val get_rate : t -> float
(** [get_rate t] is the rate of traffic flow in lane [t]. *)

val change_rate : t -> float -> t
(** [change_rate t r] is the same lane [t] but with a rate of traffic flow of
    [r]. *)

val get_output : t -> int
(** [get_output t] is the number of cars which have exited the lane since the
    start of the simulation. *)
