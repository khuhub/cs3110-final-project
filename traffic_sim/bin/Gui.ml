open Traffic_sim.Intersection

module IntersectionView = struct
  type t = {
    center : Raylib.Vector2.t;
    dim : Raylib.Vector2.t;
    intersection : Traffic_sim.Intersection.t;
  }

  type textures =
    | Ready of {
        bg : Raylib.Texture2D.t;
        red_east : Raylib.Texture2D.t;
        red_west : Raylib.Texture2D.t;
        red_north : Raylib.Texture2D.t;
        red_south : Raylib.Texture2D.t;
        yellow_east : Raylib.Texture2D.t;
        yellow_west : Raylib.Texture2D.t;
        yellow_north : Raylib.Texture2D.t;
        yellow_south : Raylib.Texture2D.t;
        green_east : Raylib.Texture2D.t;
        green_west : Raylib.Texture2D.t;
        green_north : Raylib.Texture2D.t;
        green_south : Raylib.Texture2D.t;
        car : Raylib.Texture2D.t;
      }
    | Unloaded

  let tex = ref Unloaded

  let init_textures () =
    let open Raylib in
    let transform_light img rots =
      let dimLight = 30 in
      image_resize (addr img) dimLight dimLight;
      image_rotate (addr img) (90 * rots)
    in
    let im = ref (load_image "../textures/SingleIntersection.png") in
    image_resize (addr !im) (get_screen_height ()) (get_screen_width ());
    let red_east = load_image "../textures/red_right.png" in
    let yellow_east = load_image "../textures/yellow_right.png" in
    let green_east = load_image "../textures/green_right.png" in
    let car = load_image "../textures/car.png" in
    tex :=
      Ready
        {
          bg = load_texture_from_image !im;
          red_east =
            (let img = load_image "../textures/red_right.png" in
             transform_light img 0;
             load_texture_from_image img);
          red_south =
            (let img = image_copy red_east in
             transform_light img 1;
             load_texture_from_image img);
          red_west =
            (let img = image_copy red_east in
             transform_light img 2;
             load_texture_from_image img);
          red_north =
            (let img = image_copy red_east in
             transform_light img 3;
             load_texture_from_image img);
          yellow_east =
            (let img = load_image "../textures/yellow_right.png" in
             transform_light img 0;
             load_texture_from_image img);
          yellow_south =
            (let img = image_copy yellow_east in
             transform_light img 1;
             load_texture_from_image img);
          yellow_west =
            (let img = image_copy yellow_east in
             transform_light img 2;
             load_texture_from_image img);
          yellow_north =
            (let img = image_copy yellow_east in
             transform_light img 3;
             load_texture_from_image img);
          green_east =
            (let img = load_image "../textures/green_right.png" in
             transform_light img 0;
             load_texture_from_image img);
          green_south =
            (let img = image_copy green_east in
             transform_light img 1;
             load_texture_from_image img);
          green_west =
            (let img = image_copy green_east in
             transform_light img 2;
             load_texture_from_image img);
          green_north =
            (let img = image_copy green_east in
             transform_light img 3;
             load_texture_from_image img);
          car =
            (image_resize (addr car) 100 100;
             load_texture_from_image car);
        }

  let create center dim intersection =
    init_textures ();
    { center; dim; intersection }

  let draw_light i color =
    match !tex with
    | Ready k -> (
        let open Raylib in
        let rdim = 40 in
        let cx, cy =
          (* Todo: get texture widths???? *)
          ((get_screen_width () / 2) - 15, (get_screen_height () / 2) - 15)
        in
        Traffic_sim.TrafficLight.(
          match color with
          | Green ->
              if i = 0 then
                draw_texture k.green_north (cx - rdim) (cy - rdim) Color.white
              else if i = 1 then
                draw_texture k.green_east (cx + rdim) (cy - rdim) Color.white
              else if i = 2 then
                draw_texture k.green_south (cx + rdim) (cy + rdim) Color.white
              else draw_texture k.green_west (cx - rdim) (cy + rdim) Color.white
          | Red ->
              if i = 0 then
                draw_texture k.red_north (cx - rdim) (cy - rdim) Color.white
              else if i = 1 then
                draw_texture k.red_east (cx + rdim) (cy - rdim) Color.white
              else if i = 2 then
                draw_texture k.red_south (cx + rdim) (cy + rdim) Color.white
              else draw_texture k.red_west (cx - rdim) (cy + rdim) Color.white
          | Yellow ->
              if i = 0 then
                draw_texture k.yellow_north (cx - rdim) (cy - rdim) Color.white
              else if i = 1 then
                draw_texture k.yellow_east (cx + rdim) (cy - rdim) Color.white
              else if i = 2 then
                draw_texture k.yellow_south (cx + rdim) (cy + rdim) Color.white
              else
                draw_texture k.yellow_west (cx - rdim) (cy + rdim) Color.white))
    | Unloaded -> failwith "not yet loaded"

  let draw_lights { intersection } =
    let open Traffic_sim in
    let open Raylib in
    let lights =
      List.map
        (fun a -> TrafficLight.TrafficLight.get_color a.light)
        (Intersection.list_lane_lights intersection)
    in
    List.iteri draw_light lights

  let draw_cars { intersection } =
    let open Traffic_sim in
    let open Raylib in
    let lanes =
      List.map (fun a -> a.lane) (Intersection.list_lane_lights intersection)
    in
    ()

  (** Draw a intersection centered at the given point with the given width and
      height.*)
  let draw t =
    let open Raylib in
    let open Traffic_sim.Intersection in
    match !tex with
    | Ready k ->
        draw_texture k.bg 0 0 Color.white;
        (* let str = string_of_intersection t.intersection in *)
        (* draw_text str (cx - rdim) (cy + rdim) 15 Color.raywhite; *)
        draw_lights t
    | Unloaded -> failwith "load textures."
end

module State = struct
  type mode =
    | SingleView of IntersectionView.t
    | MultiView of IntersectionView.t list

  type t = {
    mode : mode;
    steps : int;
    fpstep : int;
  }

  let draw t =
    let open Raylib in
    begin_drawing ();
    clear_background Color.black;
    (match t.mode with
    | SingleView a -> IntersectionView.draw a
    | MultiView a -> List.iter (fun x -> IntersectionView.draw x) a);

    end_drawing ()

  let setup () =
    let open Raylib in
    let open Raygui in
    load_style_default ();
    init_window 800 800 "hello world yo";
    maximize_window ();
    let w, h =
      (float_of_int (get_screen_width ()), float_of_int (get_screen_height ()))
    in
    set_target_fps 60;
    let thing =
      IntersectionView.create
        (Raylib.Vector2.create (w *. 0.5) (h *. 0.5))
        (Raylib.Vector2.create 400.0 400.0)
        Traffic_sim.Intersection.empty
    in
    { mode = SingleView thing; fpstep = 10; steps = 0 }

  let rec loop { mode; fpstep; steps } =
    match mode with
    | SingleView a ->
        let open Raylib in
        let open Traffic_sim in
        if window_should_close () then close_window () else begin_drawing ();
        draw { mode; fpstep; steps };
        if steps mod fpstep = 0 then
          let intersection = random_step a.intersection in
          loop
            {
              mode = SingleView { a with intersection };
              fpstep;
              steps = steps + 1;
            }
        else loop { mode; fpstep; steps = steps + 1 }
    | MultiView a -> failwith "Not yet implemented"
end

let main () = State.setup () |> State.loop
