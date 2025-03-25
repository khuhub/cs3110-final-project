open Car

type t

val empty_lane : t
(** A lane without any cars. *)

val peek_car : t -> Car.t
(** [peek_car t] is the car at the head of the lane [t]. *)

val push_car : t -> Car.t -> unit
(** [push_car t c] adds car [c] to the end of the queue. *)

val pop_car : t -> t
(** [pop_car t ] is the first car in the lane removed. *)

val get_rate : t -> float
(** [get_rate t] is the rate of traffic in lane [t]. *)

val change_rate : t -> float -> t
(** [change_rate t r] is the lane [t] with a rate of [r]. *)
