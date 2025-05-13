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
  num_cars : int;
}
(** AF: A value [i] of type [t] represents an intersection
    - [i.lanes] is an array of the lane-light pairs that make up the
      intersection, with each element of the list corresponding to the North,
      East, South, and West lanes respectively based on their index.
    - [i.cars_in_intersection] is the array of cars currently passing through
      the intersection.
    - [i.steps] is how many steps have elapsed.

    RI: [steps >= 0] and [List.length lanes = 4]*)

let empty () =
  let lanes =
    Array.init 4 (fun i ->
        {
          lane = Lane.empty_lane;
          light =
            (if i mod 2 = 0 then TrafficLight.create Green
             else TrafficLight.create Red);
        })
  in

  {
    lanes;
    cars_in_intersection = Array.init 4 (fun _ -> None);
    steps = 0;
    num_cars = 0;
  }

(** [add_cars lanes carlst_arr] pushes the cars in each sublist in [carlst_arr]
    to its corresponding lane. *)
let add_cars i carlst_arr =
  let num_added_cars = ref 0 in
  let rec add_cars_one_lane l = function
    | [] -> l
    | h :: t ->
        num_added_cars := !num_added_cars + 1;
        add_cars_one_lane { l with lane = Lane.push_car h l.lane } t
  in
  let lanes =
    Array.map2
      (fun lane_pair carlst -> add_cars_one_lane lane_pair carlst)
      i.lanes carlst_arr
  in
  { i with lanes; num_cars = i.num_cars + !num_added_cars }

let add_one_car i l car =
  let lane_pair = i.lanes.(l) in
  let new_lane = Lane.push_car car lane_pair.lane in
  let new_lanes = Array.copy i.lanes in
  new_lanes.(l) <- { lane_pair with lane = new_lane };
  { i with lanes = new_lanes; num_cars = i.num_cars + 1 }

let get_lane_pair i index = Array.get i.lanes index

let set_rate_one rate index i =
  let { lane; light } = get_lane_pair i index in
  { lane = Lane.change_rate lane rate; light }

let set_rate rate index i =
  let lane = get_lane_pair i index in
  Array.set i.lanes index { lane with lane = Lane.change_rate lane.lane rate };
  i

let set_rate_whole rate_arr i =
  let lane_arr = Array.mapi (fun index r -> set_rate_one r index i) rate_arr in
  { i with lanes = lane_arr }

let get_rate index i = Lane.get_rate i.lanes.(index).lane

let create carlst_arr rate_arr =
  add_cars (empty ()) carlst_arr |> set_rate_whole rate_arr

let get_steps { steps } = steps

(** [car_to_steps_left c] is how many steps a given car [c] needs to take to
    exit the intersection. *)
let car_to_steps_left c =
  match Car.get_turn c with
  | Right -> 0
  | Straight -> 1
  | Left -> 2

(** [can_enter_intersection c oncoming_lane i] is if car [c] can enter the
    intersection. *)
let can_enter_intersection c index oncoming_lane cars_in_intersection =
  match Car.get_turn c with
  | Right | Straight ->
      Array.for_all
        (fun i_car ->
          match i_car with
          | None -> true
          | Some { car; enter_lane } ->
              Car.get_turn car <> Left || enter_lane == index)
        cars_in_intersection
  | Left ->
      (match Lane.peek_car oncoming_lane.lane with
      | None -> true
      | Some c -> Car.get_turn c = Left && index < 2)
      && Array.for_all
           (fun i_car ->
             match i_car with
             | None -> true
             | Some { car; enter_lane } -> enter_lane = index)
           cars_in_intersection

(** [spawn_car i index] returns [true] with probability equal to the spawn rate
    of the lane at [index] in [i.lanes], and [false] otherwise. *)
let spawn_car i index =
  Random.float 1.0 < Lane.get_rate (get_lane_pair i index).lane

(** [increment_light lane] is the resulting lane pair [lane] after the traffic
    light has incremeneted. *)
let increment_light lane =
  { lane with light = TrafficLight.increment lane.light }

(** [increment_intersection_cars i] is the resulting intersection cars array and
    the popped cars array after one time step. *)
let increment_intersection_cars i =
  let new_arr = Array.make 4 None in
  let popped_cars = Array.make 4 None in
  Array.iteri
    (fun index car ->
      match car with
      | None -> ()
      | Some c ->
          if c.steps_left > 0 then
            Array.set new_arr
              ((index + 3) mod 4)
              (Some { c with steps_left = c.steps_left - 1 })
          else popped_cars.(index) <- Some c)
    i.cars_in_intersection;
  (new_arr, popped_cars)

(** [new_lanes i new_cars_in_intersection] is the resulting array of lanes after
    one step and the sum of the amount of cars added to each lane. *)
let increment_lanes i new_cars_in_intersection =
  let arr =
    Array.mapi
      (fun index { lane; light } ->
        match Lane.peek_car lane with
        | None -> increment_light { lane; light }
        | Some c ->
            if
              TrafficLight.can_go (car_to_steps_left c) light
              && can_enter_intersection c index
                   (get_lane_pair i ((index + 2) mod 4))
                   new_cars_in_intersection
            then (
              let steps_left = car_to_steps_left c in
              Array.set new_cars_in_intersection index
                (Some { car = c; steps_left; enter_lane = index });
              increment_light
                { lane = snd (Option.get (Lane.pop_car lane)); light })
            else increment_light { lane; light })
      i.lanes
  in
  let num_added_cars = ref 0 in
  let incremented_lanes =
    Array.mapi
      (fun index lane_pair ->
        if spawn_car i index then (
          num_added_cars := !num_added_cars + 1;
          {
            lane_pair with
            lane = Lane.push_car (Car.random_car ()) lane_pair.lane;
          })
        else lane_pair)
      arr
  in
  (incremented_lanes, !num_added_cars)

let step carlst_arr i =
  if Array.length carlst_arr <> 4 then
    raise (Invalid_argument "Must have four elements.")
  else
    let new_intersection_cars, popped_intersection_cars =
      increment_intersection_cars i
    in
    let num_popped_cars =
      Array.fold_left
        (fun acc -> function
          | None -> acc
          | Some _ -> acc + 1)
        0 popped_intersection_cars
    in
    let popped_cars =
      Array.map
        (fun x ->
          match x with
          | None -> None
          | Some { car } -> Some (Car.randomize_turn car))
        popped_intersection_cars
    in

    let incremented_lanes, num_added_cars =
      increment_lanes i new_intersection_cars
    in
    Printf.printf
      "\n\nNum cars inital: %i\nNum cars added: %i\nNum cars removed: %i\n"
      i.num_cars num_added_cars num_popped_cars;
    ( {
        lanes = incremented_lanes;
        cars_in_intersection = new_intersection_cars;
        steps = i.steps + 1;
        num_cars = i.num_cars + num_added_cars - num_popped_cars;
      },
      popped_cars )

let random_step i =
  let carlst_arr =
    Array.init 4 (fun x -> if spawn_car i x then [ Car.random_car () ] else [])
  in
  step carlst_arr i

let cars_in_intersection i =
  Array.map
    (fun car ->
      match car with
      | None -> None
      | Some { car } -> Some car)
    i.cars_in_intersection

let list_lane_lights t = List.rev (Array.fold_left (fun acc a -> a :: acc ) [] t.lanes)
let get_num_cars i = i.num_cars
