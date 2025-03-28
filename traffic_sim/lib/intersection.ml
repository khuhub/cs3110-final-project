open Car
open TrafficLight

type lane_light_pair = {
  lane : Lane.t;
  light : TrafficLight.t;
}

type intersection_car = {
  car : Car.t;
  enter_lane : int;
  steps_left : int;
}
(** A value [i] of type [intersection_car] represents a car in an intersection.
    - Invariant: [i.steps_left] must be a non-negative integer. *)

type t = {
  lanes : lane_light_pair array;
  cars_in_intersection : intersection_car option array;
  steps : int;
}
(** AF: A value [i] of type [t] represents an intersection
    - [i.lanes] is an array of the lane-light pairs that make up the
      intersection, with each element of the list corresponding to the North,
      East, South, and West lanes respectively based on their index.
    - [i.cars_in_intersection] is the array of cars currently passing through
      the intersection.
    - [i.steps] is how many steps have elapsed.

    RI: [steps >= 0] and [List.length lanes = 4]*)

let empty =
  let lanes =
    Array.init 4 (fun i ->
        {
          lane = Lane.empty_lane;
          light =
            (if i mod 2 = 0 then TrafficLight.create Red
             else TrafficLight.create Green);
        })
  in

  { lanes; cars_in_intersection = Array.init 4 (fun _ -> None); steps = 0 }

(** [add_cars lanes carlst_arr] pushes the cars in each sublist in [carlst_arr]
    to its corresponding lane. *)
let add_cars i carlst_arr =
  let rec add_cars_one_lane l = function
    | [] -> l
    | h :: t -> add_cars_one_lane { l with lane = Lane.push_car h l.lane } t
  in
  let lanes =
    Array.map2
      (fun lane_pair carlst -> add_cars_one_lane lane_pair carlst)
      i.lanes carlst_arr
  in
  { i with lanes }

let create carlst_arr = add_cars empty carlst_arr
let get_steps { steps } = steps

(** [car_to_steps_left c] is how many steps a given car [c] needs to take to
    exit the intersection. *)
let car_to_steps_left c =
  match Car.get_turn c with
  | Right -> 1
  | Straight -> 2
  | Left -> 3

(** [can_enter_intersection c oncoming_lane i] is if car [c] can enter the
    intersection. *)
let can_enter_intersection c index oncoming_lane cars_in_intersection =
  match Car.get_turn c with
  | Right | Straight ->
      Array.for_all
        (fun i_car ->
          match i_car with
          | None -> true
          | Some { car } -> Car.get_turn car <> Left)
        cars_in_intersection
  | Left ->
      (Array.for_all (fun i_car -> i_car = None) cars_in_intersection
      &&
      match Lane.peek_car oncoming_lane.lane with
      | None -> true
      | Some c -> Car.get_turn c = Left)
      && index < 2

(** [increment_light lane] is the resulting lane pair [lane] after the traffic
    light has incremeneted. *)
let increment_light lane =
  { lane with light = TrafficLight.increment lane.light }

(** [increment_intersection_cars i] is the resulting intersection cars array
    after one time step. *)
let increment_intersection_cars i =
  let new_arr = Array.make 4 None in
  Array.iteri
    (fun index car ->
      match car with
      | None -> ()
      | Some c ->
          if c.steps_left > 0 then
            Array.set new_arr
              ((c.steps_left + 3) mod 4)
              (Some { c with steps_left = c.steps_left - 1 }))
    i.cars_in_intersection;
  new_arr

(** [new_lanes i new_cars_in_intersection] is the resutling array of lanes after
    one step. *)
let increment_lanes i new_cars_in_intersection =
  Array.mapi
    (fun index { lane; light } ->
      match Lane.peek_car lane with
      | None -> increment_light { lane; light }
      | Some c ->
          if
            TrafficLight.can_go (car_to_steps_left c) light
            && can_enter_intersection c index
                 (Array.get i.lanes ((index + 2) mod 4))
                 new_cars_in_intersection
          then (
            let steps_left = car_to_steps_left c in
            Array.set new_cars_in_intersection index
              (Some { car = c; steps_left; enter_lane = index });
            increment_light
              { lane = snd (Option.get (Lane.pop_car lane)); light })
          else increment_light { lane; light })
    i.lanes

let step carlst_arr i =
  if Array.length carlst_arr <> 4 then
    raise (Invalid_argument "Must have four elements.")
  else
    let new_cars_in_intersection = increment_intersection_cars i in
    {
      lanes = increment_lanes i new_cars_in_intersection;
      cars_in_intersection = new_cars_in_intersection;
      steps = i.steps + 1;
    }

let random_step i =
  let carlst_arr =
    Array.init 4 (fun x ->
        if Random.float 1.0 < Lane.get_rate (Array.get i.lanes x).lane then
          [ Car.random_car () ]
        else [])
  in
  step carlst_arr i

let string_of_lane { lane; light } =
  Printf.sprintf "Light: %s;  Head: %s"
    (TrafficLight.string_of_traffic_light light)
    (match Lane.peek_car lane with
    | None -> "none"
    | Some c -> Car.string_of_car c)

let string_of_intersection_car = function
  | None -> ""
  | Some { car; steps_left; enter_lane } ->
      Printf.sprintf "%s%i%s" (Car.string_of_car car) enter_lane
        (string_of_int steps_left)

let string_of_intersection i =
  Printf.sprintf
    "N: [ %s ]\n\
     E: [ %s ]\n\
     S: [ %s ]\n\
     W: [ %s ]\n\n\
     In intersection: [ %s ]\n\
     Steps: %s"
    (string_of_lane (Array.get i.lanes 0))
    (string_of_lane (Array.get i.lanes 1))
    (string_of_lane (Array.get i.lanes 2))
    (string_of_lane (Array.get i.lanes 3))
    (Array.fold_left
       (fun acc car ->
         Printf.sprintf "%s | %s" acc (string_of_intersection_car car))
       "" i.cars_in_intersection)
    (string_of_int i.steps)

let list_lane_lights t = failwith "Not Yet Implemented"
