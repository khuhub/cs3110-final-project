open OUnit2
open Traffic_sim
open QCheck
open Car
open Intersection
open Lane
open TrafficLight

(** Constructs a car array where only the north lane has a car. *)
let car_list_north_only car = [| [ car ]; []; []; [] |]

(** Constructs a car array where only the south lane has a car. *)
let car_list_south_only car = [| []; []; [ car ]; [] |]

(** Constructs a car array where only the east lane has a car. *)
let car_list_east_only car = [| []; [ car ]; []; [] |]

(** Constructs a car array where north and south lanes have cars.*)
let car_list_parallel car1 car2 = [| [ car1 ]; []; [ car2 ]; [] |]

(* * Constructs a car array where east and west lanes have cars. let
   car_list_parallel car1 car2 = [| []; [ car1 ]; []; [ car2 ] |] *)

(** Constructs a car array where north and east lanes have cars. *)
let car_list_opposite car1 car2 = [| [ car1 ]; [ car2 ]; []; [] |]

(** Constructs a car array where north, south, and east lanes have cars. *)
let car_list_three car_n car_s car_e = [| [ car_n ]; [ car_e ]; [ car_s ]; [] |]

(** Retrieves the head car of the given lane direction (0=N, 1=E, 2=S, 3=W). *)
let get_intersection_head dir (inter : Intersection.t) =
  let lane_pair = Intersection.get_lane_pair inter dir in
  Lane.peek_car lane_pair.lane

(** Converts a car option to a string based on its direction. *)
let get_car_string car_opt =
  match car_opt with
  | None -> "none"
  | Some c -> (
      match Car.get_turn c with
      | Left -> "left"
      | Right -> "right"
      | Straight -> "straight")

(** Test: A car goes straight through the intersection on a green light. *)
let test_one_car_green _ =
  let car = Car.straight_car in
  let inter =
    Intersection.create (car_list_north_only car) [| 0.0; 0.0; 0.0; 0.0 |]
  in
  let inter1 = Intersection.step [| []; []; []; [] |] inter in
  let maybe_car = get_intersection_head 0 inter1 in
  assert_equal ~printer:(fun x -> x) "none" (get_car_string maybe_car)

(** Test: A car attempts to turn left on a red light and is blocked. *)
let test_one_car_red _ =
  let car = Car.left_car in
  let inter =
    Intersection.create (car_list_east_only car) [| 0.0; 0.0; 0.0; 0.0 |]
  in
  let inter1 = Intersection.step [| []; []; []; [] |] inter in
  let maybe_car = get_intersection_head 1 inter1 in
  let actual = get_car_string maybe_car in
  assert_equal ~printer:(fun x -> x) "left" actual

(** Test: A car attempts to go straight on a red light and is blocked. *)
let test_one_car_red_straight _ =
  let car = Car.straight_car in
  let inter =
    Intersection.create (car_list_east_only car) [| 0.0; 0.0; 0.0; 0.0 |]
  in
  let inter1 = Intersection.step [| []; []; []; [] |] inter in
  let maybe_car = get_intersection_head 1 inter1 in
  let actual = get_car_string maybe_car in
  assert_equal ~printer:(fun x -> x) "straight" actual

(** Test: Two cars from opposite directions go straight on green lights. *)
let test_parallel_forward_green _ =
  let inter =
    Intersection.create
      (car_list_parallel Car.straight_car Car.straight_car)
      [| 0.0; 0.0; 0.0; 0.0 |]
  in
  let inter1 = Intersection.step [| []; []; []; [] |] inter in
  assert_equal
    ~printer:(fun x -> x)
    "none"
    (get_car_string (get_intersection_head 0 inter1));
  assert_equal
    ~printer:(fun x -> x)
    "none"
    (get_car_string (get_intersection_head 2 inter1))

(** Test: A left-turning car is blocked by an oncoming straight car. *)
let test_conflict_left_vs_forward _ =
  let inter =
    Intersection.create
      (car_list_parallel Car.left_car Car.straight_car)
      [| 0.0; 0.0; 0.0; 0.0 |]
  in
  let inter1 = Intersection.step [| []; []; []; [] |] inter in
  assert_equal
    ~printer:(fun x -> x)
    "left"
    (get_car_string (get_intersection_head 0 inter1));
  assert_equal
    ~printer:(fun x -> x)
    "none"
    (get_car_string (get_intersection_head 2 inter1))

(** Test: A right turn on a yellow light is allowed. *)
let test_yellow_timing _ =
  let inter =
    Intersection.create
      (car_list_north_only Car.right_car)
      [| 0.0; 0.0; 0.0; 0.0 |]
  in
  let light = (get_lane_pair inter 0).light in
  TrafficLight.set_color light Yellow |> ignore;
  let inter1 = Intersection.step [| []; []; []; [] |] inter in
  assert_equal
    ~printer:(fun x -> x)
    "none"
    (get_car_string (get_intersection_head 0 inter1))

(* * Test: A left turn on a yellow light is not allowed. let
   test_yellow_timing_left _ = let inter = Intersection.create
   (car_list_north_only Car.left_car) [| 0.0; 0.0; 0.0; 0.0 |] in let light =
   (get_lane_pair inter 0).light in TrafficLight.set_color light Yellow |>
   ignore; let inter1 = Intersection.step [| []; []; []; [] |] inter in
   assert_equal ~printer:(fun x -> x) "left" (get_car_string
   (get_intersection_head 0 inter1)) *)

(* * Test: Two left-turning cars from opposite directions, earlier on blocks the
   other. *)
let test_two_left_fail _ =
  let inter =
    Intersection.create
      (car_list_parallel Car.left_car Car.left_car)
      [| 0.0; 0.0; 0.0; 0.0 |]
  in
  let inter1 = Intersection.step [| []; []; []; [] |] inter in
  assert_equal
    ~printer:(fun x -> x)
    "none"
    (get_car_string (get_intersection_head 0 inter1));
  assert_equal
    ~printer:(fun x -> x)
    "left"
    (get_car_string (get_intersection_head 2 inter1))

(** Test: A left-turning car is blocked, right-turning car proceeds. *)
let test_left_and_right _ =
  let inter =
    Intersection.create
      (car_list_parallel Car.left_car Car.right_car)
      [| 0.0; 0.0; 0.0; 0.0 |]
  in
  let inter1 = Intersection.step [| []; []; []; [] |] inter in
  assert_equal
    ~printer:(fun x -> x)
    "left"
    (get_car_string (get_intersection_head 0 inter1));
  assert_equal
    ~printer:(fun x -> x)
    "none"
    (get_car_string (get_intersection_head 2 inter1))

(** Test: A left-turning car is blocked, straight car proceeds. *)
let test_left_and_straight _ =
  let inter =
    Intersection.create
      (car_list_parallel Car.left_car Car.straight_car)
      [| 0.0; 0.0; 0.0; 0.0 |]
  in
  let inter1 = Intersection.step [| []; []; []; [] |] inter in
  assert_equal
    ~printer:(fun x -> x)
    "left"
    (get_car_string (get_intersection_head 0 inter1));
  assert_equal
    ~printer:(fun x -> x)
    "none"
    (get_car_string (get_intersection_head 2 inter1))

(** Test: Right turn and forward go on yellow, both succeed. *)
let test_yellow_forward_and_right _ =
  let inter =
    Intersection.create
      (car_list_parallel Car.right_car Car.straight_car)
      [| 0.0; 0.0; 0.0; 0.0 |]
  in
  let light = (get_lane_pair inter 0).light in
  TrafficLight.set_color light Yellow |> ignore;
  let light = (get_lane_pair inter 2).light in
  TrafficLight.set_color light Yellow |> ignore;
  let inter1 = Intersection.step [| []; []; []; [] |] inter in
  assert_equal
    ~printer:(fun x -> x)
    "none"
    (get_car_string (get_intersection_head 2 inter1));
  assert_equal
    ~printer:(fun x -> x)
    "none"
    (get_car_string (get_intersection_head 0 inter1))

(** Test: Left (blocked), forward (proceeds), right (on red).
    - N: left-turning car (green), will be blocked by S.
    - S: straight-going car (green), allowed to enter.
    - E: right-turning car (red), not allowed to enter.
    - Only the car from S should go through. *)
let test_three_cars_mix _ =
  let inter =
    Intersection.create
      (car_list_three Car.left_car Car.straight_car Car.right_car)
      [| 0.0; 0.0; 0.0; 0.0 |]
  in
  let inter1 = Intersection.step [| []; []; []; [] |] inter in
  assert_equal
    ~printer:(fun x -> x)
    "left"
    (get_car_string (get_intersection_head 0 inter1));
  assert_equal
    ~printer:(fun x -> x)
    "none"
    (get_car_string (get_intersection_head 2 inter1));
  assert_equal
    ~printer:(fun x -> x)
    "right"
    (get_car_string (get_intersection_head 1 inter1))

(** Property: Traffic light cycles through Red -> Green -> Yellow -> Red. *)
let prop_traffic_light_cycle_delayed =
  Test.make ~name:"Traffic light delayed cycle: Red → Green → Yellow"
    QCheck.unit (fun () ->
      let light = TrafficLight.create Red in

      let light_after_10 =
        let rec step n l =
          if n = 0 then l else step (n - 1) (TrafficLight.increment l)
        in
        step 11 light
      in

      let light_after_20 =
        let rec step n l =
          if n = 0 then l else step (n - 1) (TrafficLight.increment l)
        in
        step 11 light_after_10
      in

      let color10 = TrafficLight.get_color light_after_10 in
      let color20 = TrafficLight.get_color light_after_20 in

      if color10 <> Green || color20 <> Yellow then false else true)

let car_gen : Car.t QCheck.arbitrary =
  QCheck.oneofl [ Car.left_car; Car.right_car; Car.straight_car ]

(** Property: Lane behaves as FIFO queue. *)
let prop_lane_fifo =
  Test.make ~name:"Lane queue is FIFO" (QCheck.triple car_gen car_gen car_gen)
    (fun (c1, c2, c3) ->
      let lane = Lane.empty_lane in
      let lane =
        Lane.push_car c1 lane |> Lane.push_car c2 |> Lane.push_car c3
      in
      match Lane.pop_car lane with
      | Some (car_out, _) -> car_out = c1
      | None -> false)

(** Property: Lane rate must be non-negative and throws exception if not. *)
let prop_lane_spawn_rate_nonneg =
  Test.make ~name:"Lane spawn rate must be non-negative" float (fun f ->
      let valid = f >= 0. in
      if valid then
        try
          let _ = Lane.change_rate Lane.empty_lane f in
          true
        with Lane.Invalid_rate -> false
      else
        try
          let _ = Lane.change_rate Lane.empty_lane f in
          false
        with Lane.Invalid_rate -> true)

(** Property: The intersection's step count must increase*)
let prop_intersection_steps_increase =
  Test.make ~name:"Intersection step count increases"
    (array_of_size (Gen.int_bound 4) (list_of_size (Gen.int_bound 2) car_gen))
    (fun carlst_arr ->
      let carlst_arr =
        if Array.length carlst_arr = 4 then carlst_arr
        else Array.init 4 (fun _ -> [])
      in
      let i1 = Intersection.create carlst_arr [| 0.0; 0.0; 0.0; 0.0 |] in
      let i2 = Intersection.step carlst_arr i1 in
      Intersection.get_steps i2 > Intersection.get_steps i1)

let qcheck_tests =
  "QCheck Tests"
  >::: List.map QCheck_runner.to_ounit2_test
         [
           prop_traffic_light_cycle_delayed;
           prop_lane_fifo;
           prop_lane_spawn_rate_nonneg;
           prop_intersection_steps_increase;
         ]

let () = run_test_tt_main qcheck_tests

let unit_tests =
  "All Unit Tests"
  >::: [
         "one car green (straight)" >:: test_one_car_green;
         "one car red (left)" >:: test_one_car_red;
         "parallel forward cars green" >:: test_parallel_forward_green;
         "conflict: left vs forward" >:: test_conflict_left_vs_forward;
         "yellow: right turn allowed" >:: test_yellow_timing;
         "two left turns block each other" >:: test_two_left_fail;
         "left vs right" >:: test_left_and_right;
         "left vs straight" >:: test_left_and_straight;
         "yellow forward and right" >:: test_yellow_forward_and_right;
         "three cars: left, forward, right" >:: test_three_cars_mix;
         "one car red (straight)" >:: test_one_car_red_straight;
         (* "yellow: left turn not allowed" >:: test_yellow_timing_left; *)
       ]

let () = run_test_tt_main unit_tests
