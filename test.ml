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
  v_owner_id = 0; speed = 10.; capacity = 0; v_t = Truck; cargo = None;
  age = 1; status = Waiting; x = 0.; y = 1.23; destination = [1;2;3;2];
  v_loc = Some 1;
}

let gs1 = DataProcessing.load_file "tests/gs1.json"
let gs1_addroad1 = DataProcessing.load_file "tests/gs1_addroad1.json"
let road1 = 
{
  c_owner_id = 0;
  l_start = 6;
  l_end = 10;
  length = 274.4;
  c_age = 0;
  c_speed = 10.;
}
let road2 = 
{
  c_owner_id = 0;
  l_start = 4;
  l_end = 10;
  length = 999999.;
  c_age = 0;
  c_speed = 10.;
}

let gs1_test1 = handle_processes [AddRoad(road1)] gs1 false
let gs1_test2 = handle_processes [AddRoad(road2)] gs1 false

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

  "handle_processes: buy car, then sell"  >:: (fun _ -> assert_equal
    (handle_processes [BuyVehicle(car1);SellVehicle(car1)] generic_gs false)
    {generic_gs with
      players = [{generic_pl with money =
        (generic_pl.money -. ((1. -. sell_back_percentage) *. car_price))}]
    }
  );
  "handle_processes: sell vehicle you don't own"  >:: (fun _ -> assert_equal
    (handle_processes [SellVehicle(truck1)] generic_gs false)
    {generic_gs with
      players = [{generic_pl with money =
        (generic_pl.money +. (sell_back_percentage *. truck_price))}]
    }
  );

  "handle_processes: add road"  >:: (fun _ -> assert_equal
    (Map.find_edge gs1_test1.graph
        (get_loc 6 gs1_test1.graph) (get_loc 10 gs1_test1.graph))
    (Map.find_edge gs1_addroad1.graph
        (get_loc 6 gs1_addroad1.graph) (get_loc 10 gs1_addroad1.graph))
  );
  "handle_processes: add road unaffordable"  >:: (fun _ -> assert_raises
    Not_found
    (fun _ -> (Map.find_edge gs1_test2.graph
        (get_loc 4 gs1_test2.graph) (get_loc 10 gs1_test2.graph)))
  );

  "handle_processes: sell road"  >:: (fun _ -> assert_equal
    (Map.find_edge gs1_test1.graph
        (get_loc 6 gs1_test1.graph) (get_loc 10 gs1_test1.graph))
    (Map.find_edge gs1_addroad1.graph
        (get_loc 6 gs1_addroad1.graph) (get_loc 10 gs1_addroad1.graph))
  );

  (*SetVehicleDestination(v)::t -> handle_processes t (set_v_dest v st) road_bought
    | BuyVehicleCargo(v)::t -> handle_processes t (set_v_cargo v st) road_bought
    | AddRoad(c)::t ->
        handle_processes t (if road_bought then st else buy_connection c st) true
    | DeleteRoad(c)::t -> handle_processes t (sell_connection c st) road_bought
    | PurchaseRoad(c)::t ->
        handle_processes t (if road_bought then st else change_connection_owner c st) true
    | Pause::t-> handle_processes t ({st with paused = not st.paused}) road_bought
    | EndGame::t -> raise EndGame
    | Nothing::t -> handle_processes t st road_bought
  *)

]

let tests = "MTT Test Suite" >::: gamestate_tests
let _ = run_test_tt_main tests