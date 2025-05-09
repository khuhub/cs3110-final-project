open OUnit2
open Traffic_sim
open QCheck
open Car
open Intersection
open Lane
open TrafficLight

let string_of_array func arr =
  let elems = Array.map func arr in
  "[|" ^ String.concat "; " (Array.to_list elems) ^ "|]"

(** Constructs a car array whsere only the north lane has a car. *)
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
  let inter1 = fst (Intersection.step [| []; []; []; [] |] inter) in
  let maybe_car = get_intersection_head 0 inter1 in
  assert_equal ~printer:(fun x -> x) "none" (get_car_string maybe_car)

(** Test: A car attempts to turn left on a red light and is blocked. *)
let test_one_car_red _ =
  let car = Car.left_car in
  let inter =
    Intersection.create (car_list_east_only car) [| 0.0; 0.0; 0.0; 0.0 |]
  in
  let inter1 = fst (Intersection.step [| []; []; []; [] |] inter) in
  let maybe_car = get_intersection_head 1 inter1 in
  let actual = get_car_string maybe_car in
  assert_equal ~printer:(fun x -> x) "left" actual

(** Test: A car attempts to go straight on a red light and is blocked. *)
let test_one_car_red_straight _ =
  let car = Car.straight_car in
  let inter =
    Intersection.create (car_list_east_only car) [| 0.0; 0.0; 0.0; 0.0 |]
  in
  let inter1 = fst (Intersection.step [| []; []; []; [] |] inter) in
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
  let inter1 = fst (Intersection.step [| []; []; []; [] |] inter) in
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
  let inter1 = fst (Intersection.step [| []; []; []; [] |] inter) in
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
  let inter1 = fst (Intersection.step [| []; []; []; [] |] inter) in
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
  let inter1 = fst (Intersection.step [| []; []; []; [] |] inter) in
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
  let inter1 = fst (Intersection.step [| []; []; []; [] |] inter) in
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
  let inter1 = fst (Intersection.step [| []; []; []; [] |] inter) in
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
  let inter1 = fst (Intersection.step [| []; []; []; [] |] inter) in
  assert_equal
    ~printer:(fun x -> x)
    "none"
    (get_car_string (get_intersection_head 2 inter1));
  assert_equal
    ~printer:(fun x -> x)
    "none"
    (get_car_string (get_intersection_head 0 inter1))

(** Test: Left (blocked), forward (proceeds), right (on red). - N: left-turning
    car (green), will be blocked by S. - S: straight-going car (green), allowed
    to enter. - E: right-turning car (red), not allowed to enter. - Only the car
    from S should go through. *)
let test_three_cars_mix _ =
  let inter =
    Intersection.create
      (car_list_three Car.left_car Car.straight_car Car.right_car)
      [| 0.0; 0.0; 0.0; 0.0 |]
  in
  let inter1 = fst (Intersection.step [| []; []; []; [] |] inter) in
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
  Test.make ~name:"Traffic light delayed\n   cycle: Red → Green → Yellow"
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
  Test.make ~name:"Lane spawn rate must be\n   non-negative" float (fun f ->
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
  Test.make ~name:"Intersection step count\n   increases"
    (array_of_size (Gen.int_bound 4) (list_of_size (Gen.int_bound 2) car_gen))
    (fun carlst_arr ->
      let carlst_arr =
        if Array.length carlst_arr = 4 then carlst_arr
        else Array.init 4 (fun _ -> [])
      in
      let i1 = Intersection.create carlst_arr [| 0.0; 0.0; 0.0; 0.0 |] in
      let i2 = fst (Intersection.step carlst_arr i1) in
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
         "parallel\n   forward cars green" >:: test_parallel_forward_green;
         "conflict: left vs\n   forward" >:: test_conflict_left_vs_forward;
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

(* Car tests *)
let car_tests =
  "Car tests"
  >::: [
         ( "Car string test" >:: fun _ ->
           assert_equal "L" Car.(string_of_car left_car) );
         ( "Car string test" >:: fun _ ->
           assert_equal "R" Car.(string_of_car right_car) );
         ( "Car string test" >:: fun _ ->
           assert_equal "S" Car.(string_of_car straight_car) );
         ( "Random car string test" >:: fun _ ->
           let t = Car.(random_car () |> get_turn) in
           assert_bool "turn should be Left, Right, or Straight"
             (t = Left || t = Right || t = Straight) );
       ]

let () = run_test_tt_main car_tests

(* Lane tests *)

let lane1 = Lane.(push_car Car.left_car empty_lane)

let lane_tests =
  "Lane tests"
  >::: [
         ( "Empty lane peek" >:: fun _ ->
           assert_equal None Lane.(peek_car empty_lane) );
         ( "Empty lane pop" >:: fun _ ->
           assert_equal None Lane.(pop_car empty_lane) );
         ( "Unchanged lane output" >:: fun _ ->
           assert_equal 0 Lane.(get_output empty_lane) );
         ( "Nonempty lane peek" >:: fun _ ->
           assert_equal (Some Car.left_car) Lane.(lane1 |> peek_car) );
         ( "Nonempty lane pop" >:: fun _ ->
           assert_equal Car.left_car
             Lane.(lane1 |> pop_car |> Option.get |> fst) );
         ( "Nonempty lane pop" >:: fun _ ->
           assert_equal None
             Lane.(lane1 |> pop_car |> Option.get |> snd |> pop_car) );
         ( "Changed lane output" >:: fun _ ->
           assert_equal 1
             Lane.(lane1 |> pop_car |> Option.get |> snd |> get_output) );
         ( "Unchanged lane rate" >:: fun _ ->
           assert_equal 0. Lane.(get_rate empty_lane) );
         ( "Changed lane rate" >:: fun _ ->
           assert_equal 0.5 Lane.(change_rate empty_lane 0.5 |> get_rate) );
       ]

let () = run_test_tt_main lane_tests

(**Traffic Light tests*)

(**Test: create lights with every color*)
let test_create_colors _ =
  let red = TrafficLight.create Red in
  let green = TrafficLight.create Green in
  let yellow = TrafficLight.create Yellow in
  assert_equal Red (TrafficLight.get_color red);
  assert_equal Green (TrafficLight.get_color green);
  assert_equal Yellow (TrafficLight.get_color yellow)

(**Test: if a car can go depending on the steps left of light*)
let test_can_go _ =
  let green_pass = TrafficLight.create Green in
  let yellow_pass = TrafficLight.create Yellow in
  let red_block = TrafficLight.create Red in
  let yellow_block = TrafficLight.create Yellow |> TrafficLight.increment in
  assert_equal true (TrafficLight.can_go 0 green_pass);
  assert_equal true (TrafficLight.can_go 1 yellow_pass);
  assert_equal false (TrafficLight.can_go 0 red_block);
  assert_equal false (TrafficLight.can_go 2 yellow_block)

(**Test : incrementing light colors correctly*)
let test_increment_color _ =
  let rec increment n light =
    if n = 0 then light else increment (n - 1) (TrafficLight.increment light)
  in
  let red = increment 10 (TrafficLight.create Red) in
  let green = increment 8 (TrafficLight.create Green) in
  let yellow = increment 2 (TrafficLight.create Yellow) in
  assert_equal Green (TrafficLight.get_color (TrafficLight.increment red));
  assert_equal Yellow (TrafficLight.get_color (TrafficLight.increment green));
  assert_equal Red (TrafficLight.get_color (TrafficLight.increment yellow))

let traffic_light_test =
  "Traffic Light tests"
  >::: [
         "Create colors" >:: test_create_colors;
         "Can go" >:: test_can_go;
         "Increment color" >:: test_increment_color;
       ]

let () = run_test_tt_main traffic_light_test

(* Intersection tests *)

let print_intersection i_cars =
  let s = ref "" in
  Array.iter
    (fun o ->
      s := !s ^ "||";
      match o with
      | None -> s := !s ^ " _ "
      | Some c ->
          let car_s =
            begin
              match Car.get_turn c with
              | Left -> "L"
              | Right -> "R"
              | Straight -> "S"
            end
          in
          s := !s ^ car_s)
    i_cars;
  !s

let intersection_tests =
  "Intersection tests"
  >::: [
         ( "Unchanged intersection rate" >:: fun _ ->
           assert_bool "All rates should be 0."
             Intersection.(
               get_rate 0 (empty ()) = 0.
               && get_rate 1 (empty ()) = 0.
               && get_rate 2 (empty ()) = 0.
               && get_rate 3 (empty ()) = 0.) );
         ( "Changed intersection rate" >:: fun _ ->
           let i = Intersection.set_rate 0.5 1 (empty ()) in
           assert_bool "All rates but one should be 0."
             Intersection.(
               get_rate 0 i = 0.
               && get_rate 1 i = 0.5
               && get_rate 2 i = 0.
               && get_rate 3 i = 0.) );
         ( "Empty intersection num cars" >:: fun _ ->
           assert_equal 0
             Intersection.(get_num_cars (empty ()))
             ~printer:string_of_int );
         ( "Add two car" >:: fun _ ->
           let i =
             Intersection.(
               add_one_car
                 (add_one_car (empty ()) 0 Car.left_car)
                 1 Car.right_car)
           in
           assert_equal 2 (Intersection.get_num_cars i) );
         ( "cars_in_intersection" >:: fun _ ->
           let i =
             ref
               Intersection.(
                 create
                   [|
                     [ Car.left_car; Car.straight_car; Car.right_car ];
                     [];
                     [];
                     [];
                   |]
                   [| 0.; 0.; 0.; 0. |])
           in
           assert_equal
             [| None; None; None; None |]
             (Intersection.cars_in_intersection !i)
             ~printer:print_intersection;
           i := fst (Intersection.random_step !i);
           assert_equal
             [| Some Car.left_car; None; None; None |]
             (Intersection.cars_in_intersection !i)
             ~printer:print_intersection;
           i := fst (Intersection.random_step !i);
           assert_equal
             [| None; None; None; Some Car.left_car |]
             (Intersection.cars_in_intersection !i)
             ~printer:print_intersection;
           i := fst (Intersection.random_step !i);
           assert_equal
             [| None; None; Some Car.left_car; None |]
             (Intersection.cars_in_intersection !i)
             ~printer:print_intersection;
           i := fst (Intersection.random_step !i);
           assert_equal
             [| Some Car.straight_car; None; None; None |]
             (Intersection.cars_in_intersection !i)
             ~printer:print_intersection;
           i := fst (Intersection.random_step !i);
           assert_equal
             [| Some Car.right_car; None; None; Some Car.straight_car |]
             (Intersection.cars_in_intersection !i)
             ~printer:print_intersection );
       ]

let () = run_test_tt_main intersection_tests

(* City tests *)

(**Test: creates city with accurate dimensions*)
let city_dimension_test r c =
  "Test city dimensions" >:: fun _ ->
  assert_equal (r, c)
    City.(create r c 0.5 |> get_dimensions)
    ~printer:(fun (i, j) -> Printf.sprintf "%i, %i" i j)

(**Test: creates city with correct rates*)
let city_rate_test city r c rates =
  Printf.sprintf "Test city rate for (%i, %i)" r c >:: fun _ ->
  assert_equal rates (City.get_rate r c city)
    ~printer:(string_of_array string_of_float)

(**Test: creates city with correct number of steps*)
let city_step_number_test city steps =
  Printf.sprintf "Test city has %i ste[s]" steps >:: fun _ ->
  assert_equal steps (City.get_steps city) ~printer:string_of_int

(**Test: city dimensions preserved after step*)
let city_step_dimension_test city r c =
  Printf.sprintf "Test city dimensions after step" >:: fun _ ->
  assert_equal (r, c)
    City.(step city |> get_dimensions)
    ~printer:(fun (i, j) -> Printf.sprintf "%i, %i" i j)

(**Test: city step increases after each increment*)
let city_step_increments city steps =
  Printf.sprintf "Step %d times increments correctly" steps >:: fun _ ->
  let rec step_n c steps =
    if steps = 0 then c else step_n (City.step c) (steps - 1)
  in
  let c2 = step_n city steps in
  assert_equal
    (steps + City.get_steps city)
    (City.get_steps c2) ~printer:string_of_int

let test_city1 = City.create 1 1 0.5
let test_city2 = City.create 2 2 0.5
let test_city3 = City.create 5 5 0.5
let test_city4 = City.create 1 3 0.5

let city_tests =
  "City Tests"
  >::: [
         city_dimension_test 2 2;
         city_dimension_test 3 3;
         city_dimension_test 5 1;
         city_rate_test test_city1 0 0 [| 0.5; 0.5; 0.5; 0.5 |];
         city_rate_test test_city2 0 0 [| 0.5; 0.0; 0.0; 0.5 |];
         city_rate_test test_city2 1 1 [| 0.; 0.5; 0.5; 0. |];
         city_rate_test test_city3 3 3 [| 0.0; 0.0; 0.0; 0.0 |];
         city_rate_test test_city4 0 0 [| 0.5; 0.; 0.5; 0.5 |];
         city_step_number_test test_city1 0;
         city_step_number_test City.(step test_city1) 1;
         city_step_number_test City.(step test_city1 |> step) 2;
         city_step_dimension_test test_city1 1 1;
         city_step_dimension_test test_city2 2 2;
         city_step_increments test_city1 10;
         city_step_increments test_city3 10;
       ]

let () = run_test_tt_main city_tests
