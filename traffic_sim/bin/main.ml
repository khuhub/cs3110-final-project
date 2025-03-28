open Traffic_sim

let () = Random.self_init ()

let gencars =
  [
    Car.Car.random_car ();
    Car.Car.random_car ();
    Car.Car.random_car ();
    Car.Car.random_car ();
    Car.Car.random_car ();
    Car.Car.random_car ();
    Car.Car.random_car ();
    Car.Car.random_car ();
    Car.Car.random_car ();
    Car.Car.random_car ();
    Car.Car.random_car ();
  ]

let help =
  "Welcome to our Traffic Simulator! \n\
   USAGE: dune exec traffic_sim <steps per second: int>\n\
   Watch the flow of traffic in a four-way intersection. Cars are spawned with \
   random turning directions and go through the intersection.\n\
  \ \n"

(** Make sure the sps arg is > 0 !!! *)
let () =
  TheView.TheView.render
    (Intersection.create [| []; []; []; [] |] [| 0.2; 0.2; 0.2; 0.2 |])
    (* (Intersection.create [| []; []; gencars; gencars |] [| 0.0; 0.0; 0.0; 0.0
       |]) *)
    5
