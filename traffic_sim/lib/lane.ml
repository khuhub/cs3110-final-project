open Car

exception No_car
exception Invalid_rate

type t = {
  queue : Car.t Queue.t;
  rate : float;
}
(** AF: A value [lane] of type [t] represents a lane.
    - [lane.queue] is the queue of cars at the lane. The first car in the queue
      is the car that will exit the lane first.
    - [lane.rate] is the rate at which traffic enters the lane.

    RI: [lane.rate >= 0] *)

let empty_lane = { queue = Queue.create (); rate = 0. }

let peek_car { queue } =
  if Queue.is_empty queue then None else Some (Queue.take queue)

let push_car car { queue } = Queue.push car queue

let pop_car { queue } =
  if Queue.is_empty queue then raise No_car else Queue.pop queue

let get_rate { rate } = rate

let change_rate t rate =
  if rate < 0. then raise Invalid_rate else { t with rate }
