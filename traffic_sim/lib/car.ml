(** [turn] is a direction to move. *)
type turn =
  | Left
  | Right
  | Straight

(** Module representing a car approaching an intersection. *)
module type CarSig = sig
  type t
  (** [t] is a car defined by the direction it intends to turn. *)

  val left_car : t
  (** [left_car] is a car with the intention to turn left. *)

  val right_car : t
  (** [right_car] is a car with the intention to turn right. *)

  val straight_car : t
  (** [straight_car] is a car with the intention to continue straight. *)

  val random_car : unit -> t
  (** [random_car ()] is a car with a random direction. *)

  val get_turn : t -> turn
  (** [get_turn t] is the direction car [t] wants to turn. *)

  val get_colorid : t -> int
  (** [get_colorid t] is the color ID of car [t]. *)

  val randomize_turn : t -> t
  (** [randomize_turn t] returns [t] with a different turn. *)

  val string_of_car : t -> string
  (** [string_of_car t] string representation of car [t]. *)
end

module Car : CarSig = struct
  type t = {
    turn : turn;
    colorid : int;
  }
  (** AF: A value [car] of type [t] represents a car with it's intended
      direction represented as a [turn].

      RI: [car.colorid] must be a integer between [0] and [8] inclusive. *)

  let generate_colorid () = Random.int 9
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
  let randomize_turn t = { (random_car ()) with colorid = t.colorid }

  let string_of_car { turn } =
    match turn with
    | Left -> "L"
    | Right -> "R"
    | Straight -> "S"
end
