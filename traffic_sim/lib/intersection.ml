open Car

type lane_light_pair = {
  lane : Lane.t;
  light : TrafficLight.TrafficLight.t;
}

type t = {
  north : lane_light_pair;
  east : lane_light_pair;
  south : lane_light_pair;
  west : lane_light_pair;
  cars_in_intersection : Car.t list;
  steps : int;
}
(** AF: A value [i] of type [t] represents an intersection
    - [i.n.lane] is the [n] bound lane, where [n] is north, east, south, or
      west.
    - [i.n.light] is the traffic light associated with the n bound lane, where
      [n] is norht, east, south, or west.
    - [i.cars_in_intersection] is the list of cars currently passing through the
      intersection.
    - [i.steps] is how many steps have elapsed.

    RI: [steps >= 0]*)

let get_steps { steps } = steps
let step i lst = i
let random_step i = i
let string_of_intersection i = ""
