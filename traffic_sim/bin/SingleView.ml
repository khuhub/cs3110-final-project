open Traffic_sim
open Traffic_sim.Intersection
open Unix
open ANSITerminal

let () = Random.self_init ()

include View

(** [color col fmt] colors the string [fmt] with the color [col]. Requires: col
    is a valid RGB hex code starting with # *)
let color col fmt = Spices.(default |> fg (color col) |> build) fmt

(** [color col fmt] colors the string [fmt] with the color [col]. Requires: col
    is a valid RGB hex code starting with # *)
let color col fmt = Spices.(default |> fg (color col) |> build) fmt

(** Maps a list of elements to a list of index-element tuples, putting the head
    at (a + 1,b) and the rest at (a, b), ..., (a + length - 1, b). *)
let map_list_loc ((a, b) : int * int) q =
  List.mapi (fun index e -> ((a + index, b), e)) q

let color_of_car car =
  match Car.Car.get_colorid car with
  | 0 -> red
  | 1 -> blue
  | 2 -> cyan
  | 3 -> green
  | 4 -> magenta
  | 5 -> yellow
  | 6 -> white
  | 7 -> default
  | _ -> failwith "invalid color id"

let car_to_string car =
  let color = color_of_car car in
  match Car.Car.get_turn car with
  | Left -> "L"
  | Right -> "R"
  | Straight -> "S"

(** NOTE: W S E N order*)
let calc_traffic_flow wld =
  let lls = list_lane_lights wld in
  let arr = [| 0.0; 0.0; 0.0; 0.0 |] in
  List.iteri
    (fun i s ->
      arr.(i) <-
        float_of_int (Lane.get_output s.lane)
        /. (float_of_int (get_steps wld) +. 1.0))
    lls;
  arr

(** Turns the given lane into text, with the head of the lane at the given
    location. This text represents an east (opening to the west) lane.*)
let rec textify_queue loc (q : Intersection.lane_light_pair) =
  let light_color =
    match TrafficLight.TrafficLight.get_color q.light with
    | Green -> color "#00FF00" "G"
    | Yellow -> color "#FFFF00" "Y"
    | Red -> color "#FF0000" "R"
  in
  let rec get_queue queue =
    match Lane.pop_car queue with
    | None -> []
    | Some (c, q) -> c :: get_queue q
  in
  let carz = get_queue q.lane in
  (List.map car_to_string carz |> map_list_loc loc)
  @ [ ((fst loc - (!unit_x / 8), snd loc), light_color) ]

let rec add_lanes t lane_light_list loc =
  let lanes = List.map (textify_queue loc) lane_light_list in
  let rot90_lane lane =
    List.map (fun elem -> (rot90 t (fst elem), snd elem)) lane
  in
  let rec rot_lanes lanes =
    match lanes with
    | [] -> []
    | h :: k ->
        (rot90_lane h |> rot90_lane |> rot90_lane)
        :: List.map (fun elem -> rot90_lane elem) (rot_lanes k)
  in
  let textified_lanes = List.(lanes |> rot_lanes |> flatten) in
  let textified_lanes = List.(lanes |> rot_lanes |> flatten) in
  List.iter (fun e -> set_cell t (fst e) (snd e)) textified_lanes

let calcSize () =
  let w, h = ANSITerminal.size () in
  let smaller = if w > h then h else w in
  (smaller / 8) - (smaller / 8 mod 2)

let add_inter_cars canv wld =
  let cx, cy = center canv in
  let half = !unit_x / 16 in
  let nw_ne_se_sw =
    [|
      (cx - half, cy - half);
      (cx + half, cy - half);
      (cx + half, cy + half);
      (cx - half, cy + half);
    |]
  in
  let carz = Intersection.cars_in_intersection wld in
  Array.iter2
    (fun e1 e2 ->
      match e2 with
      | None -> ()
      | Some k -> set_cell canv e1 (car_to_string k))
    nw_ne_se_sw carz

(** [flow_color_code flow] creates the correct styling for the given traffic
    flow. *)
let flow_color_code flow =
  if flow = 0.0 then color "#00FF00"
  else if flow < 0.1 then color "#FFEEEE"
  else if flow < 0.2 then color "#FFBBBB"
  else if flow < 0.3 then color "#FF7777"
  else if flow < 0.4 then color "#FF4444"
  else if flow < 0.5 then color "#FF2222"
  else color "#FF0000"

let textify_far wld u w h =
  let canv = create_canvas u w h in
  let center = vec_add (center canv) (-1, -1) in
  let flow = calc_traffic_flow wld in
  (* let flow = calc_traffic_flow wld in *)
  set_cell canv (vec_add center (-1, -1)) "x";
  set_cell canv (vec_add center (-1, 0)) "x";
  set_cell canv (vec_add center (0, 0)) "x";
  set_cell canv (vec_add center (0, -1)) "x";
  for x = 1 to (u / w / 2) - 2 do
    set_cell canv (vec_add center (0, x)) (flow_color_code flow.(1) "|");
    set_cell canv
      (vec_add center (-1, (-1 * x) - 1))
      (flow_color_code flow.(0) "|");
    set_cell canv (vec_add center (x, -1)) (flow_color_code flow.(2) "-");
    set_cell canv
      (vec_add center ((-1 * x) - 1, 0))
      (flow_color_code flow.(3) "-")
  done;
  canv

(** [textify wld] is the representation of [wld] in the string matrix, with unit
    [u], width [w], and height [h] (in units).*)
let textify wld u w h =
  let canv = create_canvas u w h in
  let cx, cy = center canv in
  let l1_loc = (cx + (!unit_x / 8) + 1, cy - (!unit_x / 16)) in
  (* let lane_lights = Intersection.list_of_lane_lights wld in assert
     (List.length lane_lights = 4); *)
  set_cell canv (cx, cy) "+";
  set_cell canv (cx, cy) "+";
  let lb, ub = (cx - (!unit_x / 8), cx + (!unit_x / 8)) in
  for x = 0 to cx do
    for y = 0 to cy do
      if (x > lb && x < ub) && y > lb && y < ub then ()
      else (
        if x = lb || x = ub || y = lb || y = ub then
          sym_set_cell canv (x, y) "*";
        if x = cx || y = cy then sym_set_cell canv (x, y) "*")
    done
  done;
  add_lanes canv (Intersection.list_lane_lights wld) l1_loc;
  add_inter_cars canv wld;
  canv

let assert_RI t = failwith "Not yet implemented"

(** Note to self: to share code with CityView, the thing that unit_x is set to
    (48) is kind of like 8 times the value we actually want. So I've gone ahead
    and divided all those values by 8 in the above code.*)
let rec render wld sps =
  let wld = textify wld 48 1 1 in
  string_of_canvas wld
(* "Traffic Flow (cars exited / step) \n\ \ N: %f\n\ \ E: %f\n\ \ S: %f\n\ \ W:
   %f\n\ %!" flow.(0) flow.(1) flow.(2) flow.(3)); let new_wld = fst
   (Intersection.random_step wld) in (* print_string []
   (Intersection.string_of_intersection wld); *) Unix.sleepf (1. /. float_of_int
   sps); render new_wld sps *)
