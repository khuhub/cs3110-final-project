open Traffic_sim

module type TheViewSig = sig
  type t

  val render : 'a -> unit
end

module TheView : TheViewSig = struct
  type t = string array array

  (** RI : the y dimension of t is odd, so centers work out nicely, and the x
      dimension twice the length of the y dimension.*)

  let size t = (Array.length t, Array.length t.(0))

  (** [center t] is the center of t. *)
  let center t =
    let a, b = size t in
    (a / 2, b / 2)

  let set_cell (t : t) (a, b) str = t.(a).(b) <- str

  (** [unit_x] is the size of a single distance unit in the grid. Should be used
      to keep track of relative sizes. Must be divisible by 2. *)
  let unit_x = 6

  let unit_y = unit_x / 2

  (** [create_canvas a b] is a matrix with width a and height b of empty
      strings. *)
  let create_canvas w = Array.make_matrix w w " "

  (** rotates a point 90 degrees around the center of the given grid*)
  let rotate_90 t (a, b) =
    let cx, cy = center t in
    (cx - (b - cy), cy + (a - cx))

  let rec textify_queue (q : Lane.t) =
    match Lane.pop_car q with
    | None -> []
    | Some (c, q) -> c :: textify_queue q

  (** [textify wld] is the representation of [wld] in the string matrix.*)
  let textify wld =
    let canv = create_canvas 51 in
    let cx, cy = center canv in
    (* let lane_lights = Intersection.list_of_lane_lights wld in assert
       (List.length lane_lights = 4); *)
    set_cell canv (cx, cy) "+";
    let lb, ub = (cx - unit_x, cx + unit_x) in
    for x = 0 to 2 * cx do
      for y = 0 to 2 * cy do
        if (x > lb && x < ub) && y > lb && y < ub then ()
        else (
          if x = lb || x = ub || y = lb || y = ub then set_cell canv (x, y) "*";
          if x = cx || y = cy then set_cell canv (x, y) "*")
      done
    done;
    canv

  let assert_RI t = failwith "Not yet implemented"

  let render wld =
    let canv = textify wld in
    let w, h = size canv in
    for y = 0 to h - 1 do
      for x = 0 to w - 1 do
        print_string (" " ^ canv.(x).(y))
      done;
      print_endline ""
    done
end
