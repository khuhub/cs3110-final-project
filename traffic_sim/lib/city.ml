open Intersection

type t = {
  intersections : Intersection.t array array;
  steps : int;
  cars : int;
}
(** AF: A value [c] of type [t] represents a city with its intersections
    represented as the 2D array [c.intersections]. [c.steps] is the number of
    steps elapsed and [c.cars] is number of cars currently in [c].

    RI: [c.steps >= 0], [c.cars >= 0], [List.length (c.intersections) > 0],
    [List.length (c.intersections).(i) > 0] for all [i] such that
    [0 <= i < List.length (c.intersections)], [c.intersections] must be a
    rectangular matrix, and the rate for all lanes that connect with other lanes
    must be [0.].*)

let set_rate rate row col c =
  let max_r = Array.length c.intersections - 1 in
  let max_c = Array.length c.intersections.(0) - 1 in
  let i = ref c.intersections.(row).(col) in

  if row = 0 then i := Intersection.set_rate rate 0 !i;
  if row = max_r then i := Intersection.set_rate rate 2 !i;
  if col = 0 then i := Intersection.set_rate rate 3 !i;
  if col = max_c then i := Intersection.set_rate rate 1 !i;
  c.intersections.(row).(col) <- !i;
  c

let get_rate row col c =
  let intersection = c.intersections.(row).(col) in
  Array.init 4 (fun i -> Intersection.get_rate i intersection)

let create rows cols rate =
  let c =
    {
      intersections =
        Array.init_matrix rows cols (fun _ _ -> Intersection.empty ());
      steps = 0;
      cars = 0;
    }
  in
  let corners =
    [
      (0, 0);
      (0, Array.length c.intersections.(0) - 1);
      (Array.length c.intersections - 1, 0);
      (Array.length c.intersections - 1, Array.length c.intersections.(0) - 1);
    ]
  in
  List.fold_left (fun acc (r, c) -> set_rate rate r c acc) c corners

let get_steps c = c.steps
let num_cars c = c.cars

(** [step_intersection i cararr] returns the intersection at [i][j] that has
    been stepped and adds all of the cars that were popped off to [cararr] at
    [i][j] as an array in NESW order. *)
let step_intersection i j intersection cararr =
  let new_intersection_tuple = Intersection.random_step intersection in
  cararr.(i).(j) <- snd new_intersection_tuple;
  fst new_intersection_tuple

(** [move_car_after_step new_i new_j lane_index matrix new_matrix car] adds the
    car [car] to the lane at [lane_index] in the intersection at
    [new_matrix.(new_i).(new_j)].

    - Postconditon:[new_matrix.(new_i).(new_j)] is the same intersection as
      [matrix.(new_i).(new_j)] but with [car] added. *)
let move_car_after_step new_i new_j lane_index matrix new_matrix car =
  let new_intersection =
    Intersection.add_one_car matrix.(new_i).(new_j) lane_index car
  in
  new_matrix.(new_i).(new_j) <- new_intersection

(** [add_cars i j cars matrix new_matrix] takes an array of cars [cars] that
    were popped from the intersection [matrix.(i).(j)] in NESW order of lanes
    and pushes them to the correct lane of the correct intersection in
    [new_matrix]. *)
let add_cars i j cars matrix new_matrix =
  Array.iteri
    (fun index car ->
      match car with
      | None -> ()
      | Some c -> (
          try
            match index with
            | 0 -> move_car_after_step i (j - 1) 1 matrix new_matrix c
            | 1 -> move_car_after_step (i - 1) j 2 matrix new_matrix c
            | 2 -> move_car_after_step i (j + 1) 3 matrix new_matrix c
            | 3 -> move_car_after_step (i + 1) j 0 matrix new_matrix c
            | _ -> raise (Invalid_argument "Cars must have length of 4.")
          with _ -> ()))
    cars

let step c =
  let new_cars =
    Array.make_matrix
      (Array.length c.intersections)
      (Array.length c.intersections.(0))
      [||]
  in

  let new_intersections =
    Array.mapi
      (fun i row ->
        Array.mapi
          (fun j intersection -> step_intersection i j intersection new_cars)
          row)
      c.intersections
  in

  let new_intersections_cars_added = Array.copy new_intersections in

  Array.iteri
    (fun i row ->
      Array.iteri
        (fun j cars ->
          add_cars i j cars new_intersections new_intersections_cars_added)
        row)
    new_cars;

  let num_cars =
    Array.fold_left
      (fun acc i ->
        Array.fold_left (fun acc j -> acc + Intersection.get_num_cars j) acc i)
      0 new_intersections_cars_added
  in

  {
    intersections = new_intersections_cars_added;
    steps = c.steps + 1;
    cars = num_cars;
  }

let get_intersections t = List.map Array.to_list (Array.to_list t.intersections)

let get_dimensions t =
  (Array.length t.intersections, Array.length t.intersections.(0))

let add_one_car car row col lane city =
  let new_intersection =
    Intersection.add_one_car city.intersections.(row).(col) lane car
  in
  city.intersections.(row).(col) <- new_intersection;
  { city with cars = city.cars + 1 }
