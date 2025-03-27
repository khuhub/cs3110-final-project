open OUnit2
open Traffic_sim

let tests =
  "Test Suite"
<<<<<<< HEAD
  >::: [ (* ( " " >:: fun _ -> print_endline Intersection.(empty_intersection |>
            string_of_intersection) ); *) ]
=======
  >::: [
         ( " " >:: fun _ ->
           print_endline Intersection.(empty |> string_of_intersection) );
       ]
>>>>>>> 85f247a (Intersection fix)

let _ = run_test_tt_main tests
