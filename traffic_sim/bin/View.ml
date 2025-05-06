open ANSITerminal
(** Setup and helper functions for the TUI *)

type t = (style list * string) array array

let size t = (Array.length t, Array.length t.(0))

(** [center t] is the center of t. *)
let center t =
  let a, b = size t in
  (a / 2, b / 2)

(** [set_cell t (a, b) (style, str)] is the canvas with cell at [(a, b)] in the
    grid set to [(style, str)]. TEMPORARY bug fix: if a, b are out of bounds, do
    nothing. *)
let set_cell (t : t) (a, b) (style, str) =
  let w, h = size t in
  if a >= w - 1 || b >= h - 1 || a < 0 || b < 0 then ()
  else
    match style with
    | [] -> t.(a).(b) <- (fst t.(a).(b), str)
    | style -> t.(a).(b) <- (style @ [ Background Black ], str)

(** [unit_x] is the size of a single distance unit in the grid. Should be used
    to keep track of relative sizes. Must be divisible by 2. *)
let unit_x = ref (-1)

let unit_y = ref (!unit_x / 2)

(** [create_canvas unit] is a grid with the given unit. Requires: [unit] to be
    odd*)
let create_canvas unit =
  unit_x := unit;
  let w = 8 * !unit_x in
  Array.make_matrix (w + 1) (w + 1) ([ on_black ], " ")

(** rotates a point 90 degrees clockwise around the center of the given grid*)
let rot90 t (a, b) =
  let cx, cy = center t in
  (cx + (b - cy), cy - (a - cx))

(** sets four cells in a symmetrical way around the center of the grid.*)
let sym_set_cell (t : t) (a, b) str =
  set_cell t (a, b) str;
  set_cell t ((a, b) |> rot90 t) str;
  set_cell t ((a, b) |> rot90 t |> rot90 t) str;
  set_cell t ((a, b) |> rot90 t |> rot90 t |> rot90 t) str

let print_canv canv =
  let w, h = size canv in
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      print_string (fst canv.(x).(y)) (" " ^ snd canv.(x).(y))
    done;
    print_endline ""
  done
