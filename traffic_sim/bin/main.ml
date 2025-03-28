open Traffic_sim

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
  ]

let help =
  "Welcome to our Traffic Simulator! \n\
   USAGE: dune exec traffic_sim <steps per second: int>\n\
   Watch the flow of traffic in a four-way intersection. Cars are spawned with \
   random turning directions and go through the intersection.\n\
  \ \n"
(* let () = TheView.TheView.render (Intersection.create [| gencars; gencars;
   gencars; gencars |]) 5 *)
