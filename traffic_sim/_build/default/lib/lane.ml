open Car

type t = {
  queue : Car.t Queue.t;
  rate : float;
}

let empty_lane = { queue = Queue.create (); rate = 0. }
let peek_car t = Queue.take t.queue
let push_car t car = ()
let pop_car t = t
let get_rate t = t.rate
let change_rate t rate = { t with rate }
