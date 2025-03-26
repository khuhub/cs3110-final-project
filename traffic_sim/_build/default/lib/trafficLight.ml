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

  val increment : t -> t
  (** [increment t] is the resulting traffic light after [t] increments one time
      step. *)
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
    | Green -> { color = Green; steps_left = 10 }
    | Yellow -> { color = Yellow; steps_left = 2 }

  let get_color { color } = color

  let increment { color; steps_left } =
    match (color, steps_left) with
    | Red, 0 -> { color = Green; steps_left = 10 }
    | Green, 0 -> { color = Yellow; steps_left = 2 }
    | Yellow, 0 -> { color = Red; steps_left = 10 }
    | _, n -> { color; steps_left = n - 1 }
end
