open Traffic_sim
open Unix
open ANSITerminal

module type TheViewSig = sig
  type t

  val render : Intersection.t -> int -> unit
end

module TheView : TheViewSig = struct
  type t = (style list * string) array array

  (** RI : the y dimension of t is odd, so centers work out nicely, and the x
      dimension twice the length of the y dimension.*)

  let size t = (Array.length t, Array.length t.(0))

  (** [center t] is the center of t. *)
  let center t =
    let a, b = size t in
    (a / 2, b / 2)

  let set_cell (t : t) (a, b) (style, str) = t.(a).(b) <- (style, str)

  (** [unit_x] is the size of a single distance unit in the grid. Should be used
      to keep track of relative sizes. Must be divisible by 2. *)
  let unit_x = ref (-1)

  let unit_y = ref (!unit_x / 2)

  (** [create_canvas unit] is a grid with the given unit. Requires: [unit] to be
      odd*)
  let create_canvas unit =
    unit_x := unit;
    let w = 11 * !unit_x in
    Array.make_matrix (w + 1) (w + 1) ([], " ")

  (** rotates a point 90 degrees around the center of the given grid*)
  let rot90 t (a, b) =
    let cx, cy = center t in
    (cx - (b - cy), cy + (a - cx))

  let sym_set_cell (t : t) (a, b) str =
    set_cell t (a, b) str;
    set_cell t ((a, b) |> rot90 t) str;
    set_cell t ((a, b) |> rot90 t |> rot90 t) str;
    set_cell t ((a, b) |> rot90 t |> rot90 t |> rot90 t) str

  (** Maps a list of elements to a list of index-element tuples, putting the
      head at (a + 1,b) and the rest at (a, b), ..., (a + length - 1, b). *)
  let map_list_loc ((a, b) : int * int) q =
    List.mapi (fun index e -> ((a + index, b), e)) q

  let rec textify_queue loc (q : Lane.t * TrafficLight.TrafficLight.t) =
    let light_color =
      match TrafficLight.TrafficLight.get_color (snd q) with
      | Green -> ([ green ], "G")
      | Yellow -> ([ yellow ], "Y")
      | Red -> ([ red ], "R")
    in
    let rec get_queue queue =
      match Lane.pop_car queue with
      | None -> []
      | Some (c, q) -> c :: get_queue q
    in
    let carz = get_queue (fst q) in
    (List.map
       (fun e ->
         match Car.Car.get_turn e with
         | Left -> ([ cyan ], "L")
         | Right -> ([ magenta ], "R")
         | Straight -> ([ blue ], "S"))
       carz
    |> map_list_loc loc)
    @ [ ((fst loc - !unit_x, snd loc), light_color) ]

  let rec add_lanes t lane_light_list loc =
    let lanes = List.map (textify_queue loc) lane_light_list in
    let rec rot_lanes lanes =
      match lanes with
      | [] -> []
      | h :: k ->
          h
          :: rot_lanes
               (List.map
                  (fun e ->
                    List.map (fun elem -> (rot90 t (fst elem), snd elem)) e)
                  k)
    in
    let rot_lanes = rot_lanes lanes in
    List.iter (fun e -> set_cell t (fst e) (snd e)) (List.flatten rot_lanes)

  (** [textify wld] is the representation of [wld] in the string matrix.*)
  let textify wld =
    let canv = create_canvas 4 in
    let cx, cy = center canv in
    let l1_loc = (cx + !unit_x + 1, cy - (!unit_y / 2)) in
    (* let lane_lights = Intersection.list_of_lane_lights wld in assert
       (List.length lane_lights = 4); *)
    set_cell canv (cx, cy) ([], "+");
    let lb, ub = (cx - !unit_x, cx + !unit_x) in
    for x = 0 to cx do
      for y = 0 to cy do
        if (x > lb && x < ub) && y > lb && y < ub then ()
        else (
          if x = lb || x = ub || y = lb || y = ub then
            sym_set_cell canv (x, y) ([], "*");
          if x = cx || y = cy then sym_set_cell canv (x, y) ([], "*"))
      done
    done;
    add_lanes canv (Intersection.list_lane_lights wld) l1_loc;
    canv

  let assert_RI t = failwith "Not yet implemented"

  let print_canv canv =
    let w, h = size canv in
    for y = 0 to h - 1 do
      for x = 0 to w - 1 do
        print_string
          (Background Black :: fst canv.(x).(y))
          (" " ^ snd canv.(x).(y))
      done;
      print_string [ Background Black ] " ";
      print_endline ""
    done

  let rec render wld sps =
    Unix.sleepf (1. /. float_of_int sps);
    erase Screen;
    print_canv (textify wld);
    print_endline
      ("Steps: "
      ^ string_of_int (Intersection.get_steps wld)
      ^ "\tSteps Per Second: " ^ string_of_int sps);
    let new_wld = Intersection.random_step wld in
    render new_wld sps
end
