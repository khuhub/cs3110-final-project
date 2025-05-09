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
  let i = ref c.intersections.(row).(col) in

  if row = 0 then i := Intersection.set_rate rate 0 !i;
  if row = max_r then i := Intersection.set_rate rate 2 !i;
  if col = 0 then i := Intersection.set_rate rate 3 !i;
  if col = max_c then i := Intersection.set_rate rate 1 !i;
  c.intersections.(row).(col) <- !i;
  c

(*** let updated_intersection = match (row, col) with | 0, 0 ->
  Intersection.set_rate rate 0 intersection |> Intersection.set_rate rate 3 | 0,
  j when j <> max_c -> Intersection.set_rate rate 0 intersection | i, 0 when i
  <> max_r -> Intersection.set_rate rate 3 intersection | 0, j when j = max_c ->
  Intersection.set_rate rate 0 intersection |> Intersection.set_rate rate 1 | i,
  0 when i = max_r -> Intersection.set_rate rate 2 intersection |>
  Intersection.set_rate rate 3 | i, j when i = max_r && j = max_c ->
  Intersection.set_rate rate 1 intersection |> Intersection.set_rate rate 2 | i,
  j when i = max_r -> Intersection.set_rate rate 2 intersection | i, j when j =
  max_c -> Intersection.set_rate rate 1 intersection | _ -> failwith "Invalid
  intersection coordinates" in c.intersections.(row).(col) <-
  updated_intersection; c**)

let get_rate row col c =
  let intersection = c.intersections.(row).(col) in
  Array.init 4 (fun i -> Intersection.get_rate i intersection)

(* let set_rate rate row col c = let max_r = Array.length c.intersections - 1 in
   let max_c = Array.length c.intersections.(0) - 1 in

   match (row, col) with | 0, 0 -> c.intersections.(0).(0) <-
   Intersection.set_rate rate 0 c.intersections.(0).(0) |> Intersection.set_rate
   rate 3; c | 0, j when j <> max_r -> c.intersections.(0).(j) <-
   Intersection.set_rate rate 0 c.intersections.(0).(j); c | i, 0 when i <>
   max_c -> c.intersections.(i).(0) <- Intersection.set_rate rate 3
   c.intersections.(i).(0); c | 0, j when j = max_r -> c.intersections.(0).(j)
   <- Intersection.set_rate rate 0 c.intersections.(0).(j) |>
   Intersection.set_rate rate 1; c | i, 0 when i = max_c ->
   c.intersections.(i).(0) <- Intersection.set_rate rate 2
   c.intersections.(i).(0) |> Intersection.set_rate rate 3; c | i, j when i =
   max_r && j = max_c -> c.intersections.(i).(j) <- Intersection.set_rate rate 1
   c.intersections.(i).(j) |> Intersection.set_rate rate 2; c | i, j when i =
   max_r -> c.intersections.(i).(j) <- Intersection.set_rate rate 2
   c.intersections.(i).(j); c | i, j when j = max_c -> c.intersections.(i).(j)
   <- Intersection.set_rate rate 1 c.intersections.(i).(j); c | _ -> failwith
   "Invalid intersection coordinates" *)

(** set_rate rate 0 0 c |> set_rate rate 0 (Array.length c.intersections.(0) -
    1) |> set_rate rate (Array.length c.intersections - 1) 0 |> set_rate rate
    (Array.length c.intersections - 1) (Array.length c.intersections.(0) - 1)**)
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

(** [step_intersections i cararr] returns an intersection that has been stepped
    and adds all of the cars that were popped off to [cararr] *)
let step_intersections i j intersection cararr =
  let new_intersection_tuple = Intersection.random_step intersection in
  cararr.(i).(j) <- snd new_intersection_tuple;
  fst new_intersection_tuple

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
