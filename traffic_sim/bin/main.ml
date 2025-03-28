open Traffic_sim
open Core.Command
open Command_unix

let () = Random.self_init ()

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

let help =
  "Welcome to our Traffic Simulator! \n\
   Watch the flow of traffic in a four-way intersection. Cars are spawned with \
   random turning directions and go through the intersection.\n\
   By setting [set_flow_rate_into_lanes] to True, you'll be prompted to set \
   the probability of cars spawning in each of the lanes each step. Otherwise, \
   the default is 0.2. \n\
   The simulation will display the traffic flow exiting each lane. Check how \
   it compares to the traffic flow entering the lane!\n\
  \   "

open Core

let get_rate = function
  | None -> 0.2
  | Some (k : float) ->
      if Float.is_non_negative k then k
      else raise (Invalid_argument "Lane rates must be non-negative.")

let parse_lane_rates lr =
  List.to_array (List.map (String.split lr ~on:' ') ~f:float_of_string)

let get_ask_for_rates = function
  | None -> false
  | Some s -> s

let get_rates_from_cl ask_for_rates =
  if get_ask_for_rates ask_for_rates then (
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

(** Make sure the sps arg is > 0 !!! *)
let run (sps, ask_for_rates) =
  try
    if sps < 0 then raise (Invalid_argument "Sps must lowkey be positive.");
    let rates = get_rates_from_cl ask_for_rates in
    TheView.TheView.render
      (Intersection.create [| []; []; []; [] |] rates)
      (* (Intersection.create [| []; []; gencars; gencars |] [| 0.0; 0.0; 0.0;
         0.0 |]) *)
      sps
  with Invalid_argument k -> print_endline ("Oops! " ^ k)

let command =
  Command.basic ~summary:"Traffic Simulation - 4-Way Intersection"
    ~readme:(fun () -> help)
    Command.Param.(
      map
        (both
           (anon ("Steps_per_second" %: int))
           (anon (maybe ("Set_flow_rate_into_lanes" %: bool))))
        ~f:(fun sps () -> run sps))

let () = Command_unix.run ~version:"1.0" command
