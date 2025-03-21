open Lane

(** Module representing a car in the intersection *)

type car_state =
  | Approaching
  | Waiting
  | Crossing
  | Exiting

type t = {
  state : car_state;
  lane : Lane.t * Lane.t;
}
