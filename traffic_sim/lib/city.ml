open Intersection

type t = {
  intersections : Intersection.t array array;
  steps : int;
  cars : int;
}
(** AF: A value [c] of type [t] represents a city with its intersections
    represented as a 2D array. The number of steps that have elapsed and number
    of current cars is also stored.

    RI: [steps >= 0] and [cars >= 0] and [List.length intersections > 0] and
    [List.length intersections[i] > 0] for all i. Rate for all lanes in internal
    intersections is zero.*)

let set_rate rate row col c =
  let max_r = Array.length c.intersections - 1 in
  let max_c = Array.length c.intersections.(0) - 1 in

  match (row, col) with
  | 0, 0 ->
      c.intersections.(0).(0) <-
        Intersection.set_rate rate 0 c.intersections.(0).(0)
        |> Intersection.set_rate rate 3;
      c
  | 0, j when j <> max_r ->
      c.intersections.(0).(j) <-
        Intersection.set_rate rate 0 c.intersections.(0).(j);
      c
  | i, 0 when i <> max_c ->
      c.intersections.(i).(0) <-
        Intersection.set_rate rate 3 c.intersections.(i).(0);
      c
  | 0, j when j = max_r ->
      c.intersections.(0).(j) <-
        Intersection.set_rate rate 0 c.intersections.(0).(j)
        |> Intersection.set_rate rate 1;
      c
  | i, 0 when i = max_c ->
      c.intersections.(i).(0) <-
        Intersection.set_rate rate 2 c.intersections.(i).(0)
        |> Intersection.set_rate rate 3;
      c
  | i, j when i = max_r && j = max_c ->
      c.intersections.(i).(j) <-
        Intersection.set_rate rate 1 c.intersections.(i).(j)
        |> Intersection.set_rate rate 2;
      c
  | i, j when i = max_r ->
      c.intersections.(i).(j) <-
        Intersection.set_rate rate 2 c.intersections.(i).(j);
      c
  | i, j when j = max_c ->
      c.intersections.(i).(j) <-
        Intersection.set_rate rate 1 c.intersections.(i).(j);
      c
  | _ -> failwith "Invalid intersection coordinates"

let create rows cols rate =
  let c =
    {
      intersections = Array.make_matrix rows cols Intersection.empty;
      steps = 0;
      cars = 0;
    }
  in
  set_rate rate 0 0 c
  |> set_rate rate 0 (Array.length c.intersections - 1)
  |> set_rate rate (Array.length c.intersections.(0) - 1) 0
  |> set_rate rate
       (Array.length c.intersections.(0) - 1)
       (Array.length c.intersections - 1)

let step c = failwith "incomplete"
let get_steps c = c.steps
let num_cars c = c.cars

let string_of_city c =
  Array.fold_left
    (fun str row ->
      Array.fold_left
        (fun s i -> s ^ Intersection.string_of_intersection i)
        str row)
    "" c.intersections
