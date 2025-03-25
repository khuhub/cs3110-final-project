type turn =
  | Left
  | Right
  | Straight

(** Module representing a car in the intersection *)
module type CarSig = sig
  type t

  val left_car : t
  (** A left turning car *)

  val right_car : t
  (** A right turning car*)

  val straight_car : t
  (** A car going straight *)
end

module Car : CarSig with type t = turn = struct
  type t = turn

  let left_car = Left
  let right_car = Right
  let straight_car = Straight
end
