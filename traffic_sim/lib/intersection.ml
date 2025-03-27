open Car

type lane_light_pair = {
  lane : Lane.t;
  light : TrafficLight.TrafficLight.t;
}

type intersection_car = {
  car : Car.t;
  steps_left : int;
}
(** A value [i] of type [intersection_car] represents a car in an intersection.
    - Invariant: [i.steps_left] must be a non-negative integer. *)

type t = {
  lanes : lane_light_pair list;
  (*north : lane_light_pair; east : lane_light_pair; south : lane_light_pair;
    west : lane_light_pair;*)
  cars_in_intersection : intersection_car list;
  steps : int;
}
(** AF: A value [i] of type [t] represents an intersection
    - [i.lanes] is a list of the lane-light pairs that make up the intersection,
      with each element of the list corresponding to the North, East, South, and
      West lanes respectively based on their index.
    - [i.cars_in_intersection] is the list of cars currently passing through the
      intersection.
    - [i.steps] is how many steps have elapsed.

    RI: [steps >= 0] and [List.length lanes = 4]*)

let empty_intersection =
  {
    lanes =
      [
        {
          lane = Lane.empty_lane;
          light = TrafficLight.TrafficLight.create TrafficLight.Red;
        };
        {
          lane = Lane.empty_lane;
          light = TrafficLight.TrafficLight.create TrafficLight.Green;
        };
        {
          lane = Lane.empty_lane;
          light = TrafficLight.TrafficLight.create TrafficLight.Red;
        };
        {
          lane = Lane.empty_lane;
          light = TrafficLight.TrafficLight.create TrafficLight.Green;
        };
      ];
    cars_in_intersection = [];
    steps = 0;
  }

let add_cars lanes carlstlst =
  List.iter2
    (fun { lane } carlst ->
      List.iter (fun car -> Lane.push_car car lane) carlst)
    lanes carlstlst

let create lst =
  let i = empty_intersection in
  add_cars i.lanes lst;
  i

let get_steps { steps } = steps

(** [add_cars lanes carlstlst] pushes the cars in each sublist in [carlstlst] to
    its corresponding lane. *)

let step i carlstlst =
  if List.length carlstlst <> 4 then
    raise (Invalid_argument "Must have four elements.")
  else add_cars i.lanes carlstlst;
  { i with steps = i.steps + 1 }

let random_step i = i

let string_of_color = function
  | TrafficLight.Green -> "Green"
  | TrafficLight.Yellow -> "Yellow"
  | TrafficLight.Red -> "Red"

let string_of_lane { lane; light } =
  Printf.sprintf "Light: %s;  Head: %s"
    (string_of_color (TrafficLight.TrafficLight.get_color light))
    (match Lane.peek_car lane with
    | None -> "none"
    | Some c -> begin
        match c with
        | Left -> "Left car"
        | Right -> "Right car"
        | Straight -> "Straight car"
      end)

let string_of_intersection i =
  Printf.sprintf
    "North: [ %s ]\n\
     East: [ %s ]\n\
     South: [ %s ]\n\
     West: [ %s ]\n\
     In intersection: [ %s ]"
    (string_of_lane (List.nth i.lanes 0))
    (string_of_lane (List.nth i.lanes 1))
    (string_of_lane (List.nth i.lanes 2))
    (string_of_lane (List.nth i.lanes 3))
    (match i.cars_in_intersection with
    | [] -> ""
    | h :: t -> "")
