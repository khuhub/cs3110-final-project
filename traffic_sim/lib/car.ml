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

  val random_car : unit -> t
  (** A car with a random direction. *)

  val get_turn : t -> turn
  (** Gets the direction the car wants to turn. *)

  val get_colorid : t -> int
  (** Gets the color id of the car. *)

  val string_of_car : t -> string
  (** [string_of_car c] string representation of car [c]. *)
end

module Car : CarSig = struct
  type t = {
    turn : turn;
    colorid : int;
  }
  (** AF: A value [car] of type [t] represents a car with it's intended
      direction represented as a [turn].

      IR: None. *)

  let generate_colorid () = Random.int 11
  let left_car = { turn = Left; colorid = generate_colorid () }
  let right_car = { turn = Right; colorid = generate_colorid () }
  let straight_car = { turn = Straight; colorid = generate_colorid () }

  let random_car () =
    let num = Random.int 3 in
    let colorid = generate_colorid () in
    if num = 0 then { turn = Left; colorid }
    else if num = 1 then { turn = Right; colorid }
    else { turn = Straight; colorid }

  let get_turn t = t.turn
  let get_colorid t = t.colorid

  let string_of_car { turn } =
    match turn with
    | Left -> "L"
    | Right -> "R"
    | Straight -> "S"
end
