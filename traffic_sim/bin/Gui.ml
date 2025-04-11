module Intersection = struct
  type t = {
    center : Raylib.Vector2.t;
    dim : Raylib.Vector2.t;
  }

  let create center dim = { center; dim }

  let draw { center; dim } =
    Raylib.draw_rectangle_v center dim Raylib.Color.blue
end

module State = struct
  type t = {
    mouse_down : bool;
    mouse_loc : Raylib.Vector2.t;
    intersections : Intersection.t list;
  }

  let draw t =
    let open Raylib in
    begin_drawing ();
    clear_background Color.black;
    List.iter Intersection.draw t.intersections;
    end_drawing ()

  let setup () =
    let open Raylib in
    init_window 800 800 "hello world yo";
    set_target_fps 60;
    { mouse_down = false; mouse_loc = Vector2.zero (); intersections = [] }

  let rec loop state =
    let open Raylib in
    if window_should_close () then close_window () else begin_drawing ();
    draw state;
    let mouse = MouseButton.Left in
    let mouse_pos = get_mouse_position () in
    let dim = Vector2.create 20.0 20.0 in
    let intersections =
      if is_mouse_button_released mouse then
        Intersection.create mouse_pos dim :: state.intersections
      else state.intersections
    in
    loop { state with intersections }
end

let main () = State.setup () |> State.loop
