open OUnit2

(* *************** *)
(* Gamestate tests *)
(* *************** *)

let gamestate_tests = [
  "eval_int"  >:: (fun _ -> assert_equal 1 1);
  "eval_()" >:: (fun _ ->assert_equal 2 2);
]

let tests = "MTT Test Suite" >::: gamestate_tests

let _ = run_test_tt_main tests