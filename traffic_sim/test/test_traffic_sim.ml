open OUnit2
open Traffic_sim

let () = Random.self_init ()

let tests =
  "Test Suite"
  >::: [
         ( "Test Empty intersection" >:: fun _ ->
           Printf.printf "EMPTY:\n\n%s\n\n\n\n"
             Intersection.(empty |> string_of_intersection);
           assert_equal 1 1 );
         ( "Test Create intersection" >:: fun _ ->
           Printf.printf "CREATE:\n\n%s\n\n\n\n"
             Intersection.(
               create
                 (Array.init 4 (fun i ->
                      List.init i (fun x -> Car.Car.random_car ())))
               |> string_of_intersection);
           assert_equal 1 1 );
         ( "Test step" >:: fun _ ->
           Printf.printf "STEP:\n\n%s\n\n\n\n"
             Intersection.(
               empty
               |> step (Array.init 4 (fun _ -> []))
               |> string_of_intersection);
           assert_equal 1 1 );
         ( "Test step 2" >:: fun _ ->
           let i =
             Intersection.create
               (Array.init 4 (fun i ->
                    List.init i (fun x -> Car.Car.random_car ())))
           in
           Printf.printf "STEP 2:\n\n%s\n\n%s\n\n%s\n\n%s\n\n\n\n"
             (Intersection.string_of_intersection i)
             Intersection.(
               i |> step (Array.init 4 (fun _ -> [])) |> string_of_intersection)
             Intersection.(
               i
               |> step (Array.init 4 (fun _ -> []))
               |> step (Array.init 4 (fun _ -> []))
               |> string_of_intersection)
             Intersection.(
               i
               |> step (Array.init 4 (fun _ -> []))
               |> step (Array.init 4 (fun _ -> []))
               |> step (Array.init 4 (fun _ -> []))
               |> string_of_intersection);

           assert_equal 1 1 );
       ]

let _ = run_test_tt_main tests
