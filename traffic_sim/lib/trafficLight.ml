(** [color] is the color of a traffic light. *)
type color =
  | Red
  | Green
  | Yellow

module type TrafficLightSig = sig
  type t
  (** A value of type [t] is a traffic light with a current color and a
      countdown of steps before it changes to the next color. *)

  val create : color -> t
  (** [create c] is a traffic light with color [c] and the maximum number of
      steps left. *)

  val get_color : t -> color
  (** [get_color t] is the current color of traffic light [t]. *)

  val can_go : int -> t -> bool
  (** [can_go i t] returns whether or not a car can go past a traffic light [t]
      based on how many steps it will take to reach the other side. *)

  val increment : t -> t
  (** [increment t] is the resulting traffic light after [t] increments one time
      step. *)

  val set_color : t -> color -> t
  (** [set_color t c] is the resulting traffic light after setting the color of
      [t] to [c]. *)

  val get_steps_left : t -> int
  (** [get_steps_left t] returns the number of steps left for the current color
      of the traffic light*)
end

module TrafficLight : TrafficLightSig = struct
  type t = {
    color : color;
    steps_left : int;
  }
  (** AF: A value [tl] of type [t] represents a traffic light.
      - [tl.color] is its current color.
      - [tl.steps_left] is the steps left before it changes color.

      RI: [tl.steps_left >= 0]. *)

  let create col =
    match col with
    | Red -> { color = Red; steps_left = 10 }
    | Green -> { color = Green; steps_left = 8 }
    | Yellow -> { color = Yellow; steps_left = 2 }

  let get_color { color } = color

  let can_go i { color; steps_left } =
    let open Car in
    match (color, steps_left) with
    | Green, _ -> true
    | Yellow, n -> n > i
    | Red, _ -> false

  let increment { color; steps_left } =
    match (color, steps_left) with
    | Red, 0 -> { color = Green; steps_left = 8 }
    | Green, 0 -> { color = Yellow; steps_left = 2 }
    | Yellow, 0 -> { color = Red; steps_left = 10 }
    | _, n -> { color; steps_left = n - 1 }

  let set_color t color =
    match color with
    | Red -> { color = Red; steps_left = 10 }
    | Green -> { color = Green; steps_left = 10 }
    | Yellow -> { color = Yellow; steps_left = 2 }

  let get_steps_left t = t.steps_left
end
