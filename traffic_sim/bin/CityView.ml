open ANSITerminal
open Traffic_sim
include View

type mode =
  | Far
  | Mid
  | Single

type tile = Intersection of Intersection.t

let gen_tile mode inter =
  let dim = !unit_x in
  match mode with
  | Far -> SingleView.textify_far inter dim 1 1
  | Mid | Single -> SingleView.textify inter dim 1 1

(** sets the tile at the given tile (index given in units, where 0, 0 is the UL
    tile.)*)
let set_tile canv mode (u_x, u_y) tile =
  let x, y = size tile in
  let ulx, uly =
    match mode with
    | Far -> (u_x * x / 2, u_y * y / 2)
    | Mid | Single -> (u_x * x, u_y * y)
  in
  for x_add = 0 to x - 1 do
    for y_add = 0 to y - 1 do
      let x, y = (ulx + x_add, uly + y_add) in
      set_cell canv (x, y) tile.(x_add).(y_add)
    done
  done

let view_of_city city mode =
  let w, h = City.get_dimensions city in
  let canv =
    match mode with
    | Far -> create_canvas 8 (w / 2) (h / 2)
    | Mid -> create_canvas 21 w h
    | Single -> create_canvas 48 w h
  in
  let intersections = City.get_intersections city in
  List.iteri
    (fun y elem1 ->
      List.iteri
        (fun x elem2 -> set_tile canv mode (x, y) (gen_tile mode elem2))
        elem1)
    intersections;
  canv

let rec render city sps = ()
(* print_canv (view_of_city city Mid); Unix.sleepf (1. /. float_of_int sps);
   erase Screen; set_cursor 0 0; let new_wld = City.step city in render new_wld
   sps *)
