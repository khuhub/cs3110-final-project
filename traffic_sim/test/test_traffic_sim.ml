open OUnit2
open Traffic_sim

let tests =
  "Test Suite"
  >::: [
         ( " " >:: fun _ ->
           print_endline Intersection.(empty |> string_of_intersection) );
       ]

let _ = run_test_tt_main tests
