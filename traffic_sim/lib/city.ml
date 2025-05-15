open Intersection

type t = {
  intersections : Intersection.t array array;
  steps : int;
  cars : int;
}
(** AF: A value [c] of type [t] represents a city with its intersections
    represented as the 2D array [c.intersections]. [c.steps] is the number of
    steps that have elapsed and [c.cars] is number of current cars is also
    stored.

    RI: [c.steps >= 0], [c.cars >= 0], [List.length (c.intersections) > 0],
    [List.length (c.intersections).(i) > 0] for all [i] such that
    [0 <= i < List.length (c.intersections)], [c.intersections] must be a
    rectangular matrix, and the rate for all lanes that connect with other lanes
    must be [0].*)

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

(** [step_intersections i j intersection cararr] returns a stepped
    [intersection] and adds all of the cars that were popped off to [cararr] to
    index [i][j]. *)
let step_intersections i j intersection cararr =
  let new_intersection_tuple = Intersection.random_step intersection in
  cararr.(i).(j) <- snd new_intersection_tuple;
  fst new_intersection_tuple

(** [add_cars i j cars intersection_matrix] *)
let add_cars i j cars intersection_matrix =
  Array.iteri
    (fun i car ->
      match car with
      | None -> ()
      | Some c -> (
          try
            match i with
            | 0 ->
                let new_i =
                  Intersection.add_one_car intersection_matrix.(i - 1).(j) 2 c
                in
                intersection_matrix.(i - 1).(j) <- new_i
            | 1 ->
                let new_i =
                  Intersection.add_one_car intersection_matrix.(i).(j + 1) 3 c
                in
                intersection_matrix.(i).(j + 1) <- new_i
            | 2 ->
                let new_i =
                  Intersection.add_one_car intersection_matrix.(i + 1).(j) 0 c
                in
                intersection_matrix.(i + 1).(j) <- new_i
            | 3 ->
                let new_i =
                  Intersection.add_one_car intersection_matrix.(i).(j - 1) 1 c
                in
                intersection_matrix.(i).(j - 1) <- new_i
            | _ -> failwith "Invalid index"
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
          (fun j intersection -> step_intersections i j intersection new_cars)
          row)
      c.intersections
  in

  Array.iteri
    (fun i row ->
      Array.iteri (fun j cars -> add_cars i j cars new_intersections) row)
    new_cars;

  { c with intersections = new_intersections; steps = c.steps + 1 }

let get_intersections t = List.map Array.to_list (Array.to_list t.intersections)

let get_dimensions t =
  (Array.length t.intersections, Array.length t.intersections.(0))
