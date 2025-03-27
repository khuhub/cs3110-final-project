open Car

exception Invalid_rate

type t = {
  queue : Car.t list;
  rate : float;
}
(** AF: A value [lane] of type [t] represents a lane.
    - [lane.queue] is the queue of cars at the lane. The first car in the queue
      is the car that will exit the lane first.
    - A queue is represented by a list of cars, where [[x1, x2, ... , xn]] is
      the queue with head [x1] and tail [xn]
    - [lane.rate] is the rate at which traffic enters the lane.

    RI: [lane.rate >= 0] *)

let empty_lane = { queue = []; rate = 0. }

let peek_car { queue } =
  match queue with
  | [] -> None
  | h :: t -> Some h

let push_car (car : Car.t) t = { t with queue = t.queue @ [ car ] }

let pop_car { queue; rate } =
  match queue with
  | [] -> None
  | h :: t -> Some (h, { queue = t; rate })

let get_rate { rate } = rate

let change_rate t rate =
  if rate < 0. then raise Invalid_rate else { t with rate }
