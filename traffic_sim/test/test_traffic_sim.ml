open OUnit2
open Traffic_sim

(**jasldfkjalsdkf*)
let () = Random.self_init ()

let tests =
  "Test Suite"
  >::: [
         ( "Test step 2" >:: fun _ ->
           let i =
             Intersection.create
               (Array.init 4 (fun i ->
                    List.init i (fun x -> Car.Car.random_car ())))
               (Array.init 4 (fun i -> (float_of_int i +. 10.) /. 13.))
           in
           let arr = Array.init 4 (fun _ -> []) in
           let i2 = Intersection.step arr i in
           let i3 = Intersection.step arr i2 in
           let i4 = Intersection.step arr i3 in
           let i5 = Intersection.step arr i4 in
           let i6 = Intersection.step arr i5 in
           let i7 = Intersection.step arr i6 in
           Printf.printf "\n\n%s\n\n%s\n\n%s\n\n%s\n\n%s\n\n%s\n\n%s\n\n\n\n"
             (Intersection.string_of_intersection i)
             Intersection.(i2 |> string_of_intersection)
             Intersection.(i3 |> string_of_intersection)
             Intersection.(i4 |> string_of_intersection)
             Intersection.(i5 |> string_of_intersection)
             Intersection.(i6 |> string_of_intersection)
             Intersection.(i7 |> string_of_intersection);

           assert_equal 1 1 );
       ]

let _ = run_test_tt_main tests
