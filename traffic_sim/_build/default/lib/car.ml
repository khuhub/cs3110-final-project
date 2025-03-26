(** [turn] is a direction to move. *)
type turn =
  | Left
  | Right
  | Straight

(** Module type representing a car approaching an intersection. *)
module type CarSig = sig
  type t
  (** [t] is a car defined by the direction it intends to turn. *)

  val left_car : t
  (** A car with the intention to turn left. *)

  val right_car : t
  (** A car with the intention to turn right. *)

  val straight_car : t
  (** A car with the intention to continue straight. *)
end

module Car : CarSig with type t = turn = struct
  type t = turn
  (** AF: A value [car] of type [t] represents a car with it's intended
      direction represented as a [turn].

      IR: None. *)

  let left_car = Left
  let right_car = Right
  let straight_car = Straight
end
