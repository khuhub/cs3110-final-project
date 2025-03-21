include CarQueue

(**Module representing a lane in the intersection*)

type num = int
type queue = CarQueue.t
type rate = int

exception Invalid_route

val make_route : num -> num -> num * num
(** [make_route x y] creates a tuple of lane numbers representing a route from
    the start [x] to the destination [y]. Throws a [Invalid_route] if the route
    is invalid.*)

val set_rate_enter : int -> rate
(** [set_rate_enter x]* sets the rate that cars enter the intersection to [x]*)
