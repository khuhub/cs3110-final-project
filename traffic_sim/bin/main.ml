open Traffic_sim
open Core
open Command_unix

let () = Random.self_init ()

(** just for testing?*)
let gencars =
  [
    Car.Car.random_car ();
    Car.Car.random_car ();
    Car.Car.random_car ();
    Car.Car.random_car ();
    Car.Car.random_car ();
    Car.Car.random_car ();
    Car.Car.random_car ();
    Car.Car.random_car ();
    Car.Car.random_car ();
    Car.Car.random_car ();
    Car.Car.random_car ();
  ]

let help_single =
  "Watch the flow of traffic in a four-way intersection. Cars are spawned with \
   random turning directions and go through the intersection.\n\
   The default rate of flow is 0.2 cars/steps \n\
   The simulation will display the traffic flow exiting each lane. Check how \
   it compares to the traffic flow entering the lane!\n\
  \   "

let help_city =
  "Watch the flow of traffic through a city. Cars are spawned at the corners \
   of the city.\n\
   Set the size of the city and the flow in the entry points. \n\
  \   "

let get_rate = function
  | None -> 0.2
  | Some (k : float) ->
      if Float.is_non_negative k then k
      else raise (Invalid_argument "Lane rates must be non-negative.")

let parse_lane_rates lr =
  List.to_array (List.map (String.split lr ~on:' ') ~f:float_of_string)

let get_ask_for = function
  | None -> false
  | Some s -> s

let parse_traffic_string str =
  let strlst =
    match str with
    | None -> []
    | Some k -> String.to_list k
  in
  let f elem =
    match elem with
    | 'R' -> Car.Car.right_car
    | 'L' -> Car.Car.left_car
    | 'S' -> Car.Car.straight_car
    | '*' -> Car.Car.random_car ()
    | k ->
        raise
          (Invalid_argument
             ("Unexpected character " ^ String.of_char k ^ " when entering cars"))
  in
  List.map strlst ~f

let get_rates_from_cl ask_for_rates =
  if ask_for_rates then (
    let arr = [| 0.0; 0.0; 0.0; 0.0 |] in
    print_endline "Enter North Rate:";
    arr.(0) <- get_rate (read_float_opt ());
    print_endline "Enter East Rate:";
    arr.(1) <- get_rate (read_float_opt ());
    print_endline "Enter South Rate:";
    arr.(2) <- get_rate (read_float_opt ());
    print_endline "Enter West Rate:";
    arr.(3) <- get_rate (read_float_opt ());
    arr)
  else [| 0.2; 0.2; 0.2; 0.2 |]

let get_traffic_from_cl ask_for_traffic =
  if ask_for_traffic then (
    let arr = [| []; []; []; [] |] in
    print_endline
      "Enter initial traffic in each lane. \n\
       (R) is a right-turning car, \n\
       (L) is left-turning, \n\
       (S) is going straight, \n\
       (*) is a random turn direction.\n\
       Enter as one string without any other characters (e.g. SSLRR*) ";
    print_endline "Enter North Traffic:";
    arr.(0) <- parse_traffic_string In_channel.(input_line stdin);
    print_endline "Enter East Traffic:";
    arr.(1) <- parse_traffic_string In_channel.(input_line stdin);
    print_endline "Enter South Traffic:";
    arr.(2) <- parse_traffic_string In_channel.(input_line stdin);
    print_endline "Enter West Traffic:";
    arr.(3) <- parse_traffic_string In_channel.(input_line stdin);
    arr)
  else [| []; []; []; [] |]

(** Make sure the sps arg is > 0 !!! *)
let run_single sps ask_for_rates ask_for_traffic =
  try
    if sps < 0 then raise (Invalid_argument "Sps must lowkey be positive.");
    let rates = get_rates_from_cl ask_for_rates in
    let cars = get_traffic_from_cl ask_for_traffic in
    SingleView.render
      (Intersection.create cars rates)
      (* (Intersection.create [| []; []; gencars; gencars |] [| 0.0; 0.0; 0.0;
         0.0 |]) *)
      sps
  with Invalid_argument k -> print_endline ("Oops! " ^ k)

let run_city rows cols rate sps =
  CityView.render (City.create rows cols rate) sps

let single =
  Command.basic ~summary:"Traffic Simulation - 4-Way Intersection"
    ~readme:(fun () -> help_single)
    (let%map_open.Command useflow =
       flag "-f" no_arg ~doc:"specify traffic flow in"
     and usetraffic = flag "-t" no_arg ~doc:"specify initial traffic"
     and sps = anon (maybe_with_default 5 ("Steps per second" %: int)) in
     fun () -> run_single sps useflow usetraffic)

let city =
  Command.basic ~summary:"Traffic Simulation - City Grid"
    ~readme:(fun () -> help_city)
    (let%map_open.Command rows = anon ("Rows" %: int)
     and cols = anon ("Columns" %: int)
     and rate = anon ("Rate" %: float)
     and sps = anon (maybe_with_default 1 ("Steps per second" %: int)) in
     fun () -> run_city rows cols rate sps)

let command =
  Command.group ~summary:"Simulate Traffic"
    [ ("single", single); ("city", city) ]

let () = Command_unix.run command
