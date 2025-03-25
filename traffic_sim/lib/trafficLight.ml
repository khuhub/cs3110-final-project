type color =
  | Red
  | Green
  | Yellow

module type TrafficLightSig = sig
  type t
  (** A value of type [t] is a traffic light. *)

  val create : color -> t
  (** [create c] creates a traffic light with the given color. *)

  val get_color : t -> color
  (** [get_color t] is the current color of traffic light [t]. *)

  val increment : t -> t
  (** [increment t] is the traffic light [t] after incrementing one time step.
  *)
end

module TrafficLight : TrafficLightSig = struct
  type t = color * int
  (** AF: A value of [(c, i)] is a traffic light with color [c] and [i] steps
      left before it changes color. RI: [i>=0]. *)

  let create col =
    match col with
    | Red -> (Red, 0)
    | Green -> (Green, 0)
    | Yellow -> (Yellow, 0)

  let get_color t =
    match t with
    | Red, k -> Red
    | Green, k -> Green
    | Yellow, k -> Yellow

  let increment t =
    match t with
    | Red, 0 -> (Green, 10)
    | Green, 0 -> (Yellow, 2)
    | Yellow, 0 -> (Red, 10)
    | Red, n -> (Red, n - 1)
    | Green, n -> (Green, n - 1)
    | Yellow, n -> (Yellow, n - 1)
end
