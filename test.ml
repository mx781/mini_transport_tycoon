open OUnit2
open GameElements
open Player
open Engine

(* *************** *)
(* Gamestate tests *)
(* *************** *)

let generic_pl = {p_id = 0; p_type = Human; money = 5000.}
let broke_pl = {p_id = 0; p_type = Human; money = 0.}
let generic_gs = 
{
  vehicles = [];
  graph = Map.empty;
  players = [generic_pl];
  paused = false;
  game_age = 0;
}
let generic_gs_broke = 
{
  generic_gs with players = [broke_pl]
}

let car1 = {
  v_owner_id = 0; speed = 5.; capacity = 100; v_t = Car; cargo = None;
  age = 100; status = Driving; x = 250.; y = 250.; destination = []; 
  v_loc = None;
}
let truck1 = {
  v_owner_id = -1; speed = 10.; capacity = 0; v_t = Truck; cargo = None;
  age = 1; status = Waiting; x = 0.; y = 1.23; destination = [1;2;3;2]; 
  v_loc = Some 1;
}

let gamestate_tests = [
  "handle_processes: empty"  >:: (fun _ -> assert_equal 
    (handle_processes [] generic_gs false)
    generic_gs
  );
  "handle_processes: buy car"  >:: (fun _ -> assert_equal 
    (handle_processes [BuyVehicle(car1)] generic_gs false)
    {generic_gs with vehicles = [car1]; players = [{generic_pl with money =
      (generic_pl.money -. car_price)}]}
  );
  "handle_processes: buy car, can't afford"  >:: (fun _ -> assert_equal 
    (handle_processes [BuyVehicle(car1)] generic_gs_broke false)
    generic_gs_broke
  );
  "handle_processes: buy truck + car"  >:: (fun _ -> assert_equal 
    (handle_processes
      [BuyVehicle(truck1); BuyVehicle(car1)] generic_gs false)
    {generic_gs with
      vehicles = [car1;truck1];
      players = [{generic_pl with money = 
        (generic_pl.money -. (car_price +. truck_price))}]
    }
  );
]

let tests = "MTT Test Suite" >::: gamestate_tests

let _ = run_test_tt_main tests