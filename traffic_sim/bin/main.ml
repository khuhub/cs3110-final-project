open Traffic_sim
open Minttea
open Spices
open Leaves

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

let rec list_from_string str : string list =
  String.fold_right (fun a acc -> String.make 1 a :: acc) str []

let get_rate = function
  | None -> 0.2
  | Some (k : float) ->
      if k >= 0.0 then k
      else raise (Invalid_argument "Lane rates must be non-negative.")

let parse_lane_rates lr =
  Array.of_list (List.map float_of_string (String.split_on_char ' ' lr))

let get_ask_for = function
  | None -> false
  | Some s -> s

let parse_traffic_string str =
  let str = list_from_string str in
  let f elem =
    match elem with
    | "R" -> Car.Car.right_car
    | "L" -> Car.Car.left_car
    | "S" -> Car.Car.straight_car
    | "*" -> Car.Car.random_car ()
    | k ->
        raise
          (Invalid_argument ("Unexpected character " ^ k ^ " when entering cars"))
  in
  List.map f str

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

(* let get_traffic_from_cl ask_for_traffic = if ask_for_traffic then ( let arr =
   [| []; []; []; [] |] in print_endline "Enter initial traffic in each lane.\n\
   \ \n\ \ (R) is a right-turning car, \n\ \ (L) is left-turning, \n\ \ (S) is
   going\n\ \ straight, \n\ \ (*) is a random turn direction.\n\ \ Enter as one
   string without\n\ \ any other characters (e.g. SSLRR*) "; print_endline
   "Enter North Traffic:"; arr.(0) <- parse_traffic_string
   In_channel.(input_line stdin); print_endline "Enter East Traffic:"; arr.(1)
   <- parse_traffic_string In_channel.(input_line stdin); print_endline "Enter
   South Traffic:"; arr.(2) <- parse_traffic_string In_channel.(input_line
   stdin); print_endline "Enter West Traffic:"; arr.(3) <- parse_traffic_string
   In_channel.(input_line stdin); arr) else [| []; []; []; [] |] *)

(* * Make sure the sps arg is > 0 !!! let run_single sps ask_for_rates
   ask_for_traffic = try if sps < 0 then raise (Invalid_argument "Sps must
   lowkey be positive."); let rates = get_rates_from_cl ask_for_rates in let
   cars = get_traffic_from_cl ask_for_traffic in SingleView.render
   (Intersection.create cars rates) (* (Intersection.create [| []; []; gencars;
   gencars |] [| 0.0; 0.0; 0.0; 0.0 |]) *) sps with Invalid_argument k -> "Oops!
   " ^ k *)

let run_city rows cols rate sps =
  CityView.render (City.create rows cols rate) sps

type start_menu = {
  selected : int;
  options : string list;
}

type single_menu = {
  current_prompt : int;
  prompts : string array;
  input : Text_input.t;
  rates : float list;
  cars : string list;
}

type city_menu = { selected : int }

type simulation = {
  intersection : Intersection.t;
  spf : float;
  last_frame : Ptime.t;
}

type section =
  | Start_menu of start_menu
  | Single_menu of single_menu
  | City_menu of city_menu
  | Simulation of simulation

type model = { section : section }

let initial_intersection =
  {
    intersection =
      Intersection.create [| []; []; []; [] |] [| 0.2; 0.2; 0.2; 0.2 |];
    spf = 0.1;
    last_frame = Ptime_clock.now ();
  }

let create_intersection rates cars =
  let cars = List.map parse_traffic_string cars in
  let cars = Array.of_list cars in
  let rates = Array.of_list rates in
  {
    intersection = Intersection.create cars rates;
    spf = 0.1;
    last_frame = Ptime_clock.now ();
  }

let initial_model =
  { section = Start_menu { selected = 0; options = [ "Single"; "City" ] } }

let initial_single_menu =
  {
    current_prompt = 0;
    prompts =
      [|
        "North Rate";
        "East Rate";
        "South Rate";
        "West Rate";
        "North Cars";
        "East Cars";
        "South Cars";
        "West Cars";
      |];
    input = Text_input.empty ();
    cars = [];
    rates = [];
  }

let init _ = Command.Seq [ Enter_alt_screen; Hide_cursor ]

let page_down (screen : start_menu) =
  {
    screen with
    selected = (screen.selected + 1) mod List.length screen.options;
  }

let page_up (screen : start_menu) =
  let len = List.length screen.options in
  { screen with selected = (len + (screen.selected - 1)) mod len }

let next_state (screen : single_menu) =
  try
    let input = String.trim @@ Text_input.current_text screen.input in
    if screen.current_prompt < 4 then
      {
        section =
          Single_menu
            {
              screen with
              current_prompt = screen.current_prompt + 1;
              rates = screen.rates @ [ float_of_string @@ String.trim input ];
              input = Text_input.empty ();
            };
      }
    else if screen.current_prompt < 7 then
      {
        section =
          Single_menu
            {
              screen with
              current_prompt = screen.current_prompt + 1;
              cars = screen.cars @ [ input ];
              input = Text_input.empty ();
            };
      }
    else
      let screen = { screen with cars = screen.cars @ [ input ] } in
      { section = Simulation (create_intersection screen.rates screen.cars) }
  with Failure k -> { section = Single_menu screen }

let update event model =
  match model.section with
  | Start_menu t -> (
      match event with
      | Event.KeyDown Down ->
          ({ section = Start_menu (page_down t) }, Command.Noop)
      | Event.KeyDown Up -> ({ section = Start_menu (page_up t) }, Command.Noop)
      | Event.KeyDown enter -> (
          match t.selected with
          | 0 -> ({ section = Single_menu initial_single_menu }, Command.Noop)
          | 1 -> (model, Command.Noop)
          | _ -> (model, Command.Noop))
      | _ -> (model, Command.Noop))
  | Single_menu t -> (
      match event with
      | e ->
          if e = Event.KeyDown Space then
            ({ section = Simulation initial_intersection }, Command.Noop)
          else if e = Event.KeyDown Enter then (next_state t, Command.Noop)
          else
            let text = Text_input.update t.input e in
            ({ section = Single_menu { t with input = text } }, Command.Noop))
  | City_menu t -> (model, Command.Noop)
  | Simulation t -> (
      let new_model =
        {
          section =
            Simulation
              {
                t with
                intersection = fst (Intersection.random_step t.intersection);
                last_frame = Ptime_clock.now ();
              };
        }
      in
      match event with
      | Event.KeyDown (Key "q") -> (new_model, Command.Quit)
      | Event.Frame now ->
          let delta = Ptime.diff now t.last_frame in
          let delta = Float.abs (Ptime.Span.to_float_s delta) in
          if delta >= t.spf then (new_model, Command.Noop)
          else (model, Command.Noop)
      | _ -> (new_model, Command.Noop))

let highlight fmt = Spices.(default |> fg (color "#FF06B7") |> build) fmt
let hint fmt = Spices.(default |> fg (color "241") |> build) fmt

let view model =
  match model.section with
  | Simulation t ->
      Format.sprintf "%s \n%s"
        (SingleView.render t.intersection 5)
        (hint "Press q to quit.")
  | Start_menu t ->
      let choices =
        List.mapi
          (fun idx choice ->
            let checked = idx = t.selected in
            let checkbox = Forms.checkbox ~checked choice in
            if checked then highlight "%s" checkbox else checkbox)
          t.options
        |> String.concat "\n  "
      in
      Format.sprintf {| Select a simulation mode: 
  %s |} choices
  | Single_menu t ->
      Format.sprintf {|Enter %s: 
  %s 
%s|}
        t.prompts.(t.current_prompt)
        (Text_input.view t.input)
        (hint
           "Press Spacebar to skip and use defaults (0.2 cars/sec in all lanes \
            and no initial cars)")
  | _ -> "Hello World!"

let () = Minttea.app ~init ~update ~view () |> Minttea.start ~initial_model
