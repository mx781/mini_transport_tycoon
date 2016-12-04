open OUnit2
open GameElements
open InputProcessing
open Player
open Engine

(* Tuple helpers *)
let f (x, _, _) = x
let s (_,y,_) = y
let t (_,_,z) = z

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

let car2 = {
  v_owner_id = 0; speed = 4.; capacity = 25; v_t = Car;
  cargo = Some {t = Lumber; quantity = 25}; age = 37; status = Waiting;
  x = 510.; y = 320.; destination = []; v_loc = Some 0;
}

let gs1 = DataProcessing.load_file "tests/gs1.json"
let gs1_addroad1 = DataProcessing.load_file "tests/gs1_addroad1.json"
let gs2 = DataProcessing.load_file "tests/gs2.json"
let gs2_buycargo1 = DataProcessing.load_file "tests/gs2_buycargo1.json"
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
let road3 = 
{
  c_owner_id = -1;
  l_start = 5;
  l_end = 8;
  length = 158.1;
  c_age = 0;
  c_speed = 3.0;
}
let road4 = 
{
  c_owner_id = -1;
  l_start = 6;
  l_end = 7;
  length = 186.8;
  c_age = 0;
  c_speed = 3.0;
}

let gs1_test1 = handle_processes [AddRoad(road1)] gs1 false
let gs1_test2 = handle_processes [AddRoad(road2)] gs1 false
let gs1_test3 = handle_processes [DeleteRoad(road1)] gs1_addroad1 false
let gs1_test4 = handle_processes 
  [PurchaseRoad({road3 with c_owner_id = 0})] gs1 false
let gs1_test5 = handle_processes
  [PurchaseRoad({road3 with c_owner_id = 0});
  PurchaseRoad({road4 with c_owner_id = 0})] gs1 false
let gs1_test6 = handle_processes
  [PurchaseRoad({road4 with c_owner_id = 0});
  PurchaseRoad({road3 with c_owner_id = 0})] gs1 false
let gs1_test7 = handle_processes [Pause] gs1 false
let gs1_test8 = handle_processes [Pause;Pause] gs1 false
let gs1_test9 = handle_processes [PurchaseRoad({road3 with c_owner_id = 0});
  PurchaseRoad({road3 with c_owner_id = 0})] gs1 false

let gs2_test1 = handle_processes [BuyVehicleCargo(car2)] gs2 false

let gamestate_tests = [
  "handle_processes: empty" >:: (fun _ -> assert_equal
    (handle_processes [] generic_gs false)
    generic_gs
  );
  "handle_processes: buy car" >:: (fun _ -> assert_equal
    (handle_processes [BuyVehicle(car1)] generic_gs false)
    {generic_gs with vehicles = [car1]; players = [{generic_pl with money =
      (generic_pl.money -. car_price)}]}
  );
  "handle_processes: buy car, can't afford" >:: (fun _ -> assert_equal
    (handle_processes [BuyVehicle(car1)] generic_gs_broke false)
    generic_gs_broke
  );
  "handle_processes: buy truck + car" >:: (fun _ -> assert_equal
    (handle_processes
      [BuyVehicle(truck1); BuyVehicle(car1)] generic_gs false)
    {generic_gs with
      vehicles = [car1;truck1];
      players = [{generic_pl with money =
        (generic_pl.money -. (car_price +. truck_price))}]
    }
  );

  "handle_processes: buy car, then sell" >:: (fun _ -> assert_equal
    (handle_processes [BuyVehicle(car1);SellVehicle(car1)] generic_gs false)
    {generic_gs with
      players = [{generic_pl with money =
        (generic_pl.money -. ((1. -. sell_back_percentage) *. car_price))}]
    }
  );
  "handle_processes: sell vehicle you don't own" >:: (fun _ -> assert_equal
    (handle_processes [SellVehicle(truck1)] generic_gs false)
    {generic_gs with
      players = [{generic_pl with money =
        (generic_pl.money +. (sell_back_percentage *. truck_price))}]
    }
  );

  "handle_processes: add road" >:: (fun _ -> assert_equal
    (Map.find_edge gs1_test1.graph
        (get_loc 6 gs1_test1.graph) (get_loc 10 gs1_test1.graph))
    (Map.find_edge gs1_addroad1.graph
        (get_loc 6 gs1_addroad1.graph) (get_loc 10 gs1_addroad1.graph))
  );
  "handle_processes: add road unaffordable" >:: (fun _ -> assert_raises
    Not_found
    (fun _ -> (Map.find_edge gs1_test2.graph
        (get_loc 4 gs1_test2.graph) (get_loc 10 gs1_test2.graph)))
  );

  "handle_processes: delete road" >:: (fun _ -> assert_raises
    Not_found
    (fun _ -> (Map.find_edge gs1_test3.graph
        (get_loc 6 gs1_test3.graph) (get_loc 10 gs1_test3.graph)))
  );
  "handle_processes: delete unexisting road" >:: (fun _ -> assert_raises
    Not_found
    (fun _ -> handle_processes [DeleteRoad(road2)] gs1_addroad1 false)
  );

  "handle_processes: purchase road" >:: (fun _ -> assert_equal
    (let e = Map.find_edge gs1_test4.graph 
      (get_loc 5 gs1_test4.graph) (get_loc 8 gs1_test4.graph)
    in (s e).c_owner_id)
    0
  );
  "handle_processes: purch. 2rds A - 1st purchased" >:: (fun _ -> assert_equal
    (let e = Map.find_edge gs1_test5.graph 
      (get_loc 5 gs1_test5.graph) (get_loc 8 gs1_test5.graph)
    in (s e).c_owner_id)
    0
  );
  "handle_processes: purch. 2rds A - 2nd unaffordable" >:: (fun _ ->assert_equal
    (let e = Map.find_edge gs1_test5.graph 
      (get_loc 6 gs1_test5.graph) (get_loc 7 gs1_test5.graph)
    in (s e).c_owner_id)
    ~-1
  );
  "handle_processes: purch. 2rds B - 1st purchased" >:: (fun _ -> assert_equal
    (let e = Map.find_edge gs1_test6.graph 
      (get_loc 6 gs1_test5.graph) (get_loc 7 gs1_test5.graph)
    in (s e).c_owner_id)
    0
  );
  "handle_processes: purch. 2rds B - 2nd unaffordable" >:: (fun _ ->assert_equal
    (let e = Map.find_edge gs1_test6.graph 
      (get_loc 5 gs1_test6.graph) (get_loc 8 gs1_test6.graph)
    in (s e).c_owner_id)
    ~-1
  );
  "handle_processes: purchase same road twice: road" >:: (fun _ -> assert_equal
    (let e = Map.find_edge gs1_test9.graph 
      (get_loc 5 gs1_test9.graph) (get_loc 8 gs1_test9.graph)
    in (s e).c_owner_id)
    0
  );

  "handle_processes: pause" >:: (fun _ -> assert_equal
    gs1_test7
    {gs1 with paused = true}
  );
  "handle_processes: pause/unpause" >:: (fun _ -> assert_equal
    gs1_test8
    gs1
  );

  "handle_processes: nothing" >:: (fun _ -> assert_equal
    (handle_processes [Nothing] gs1 false)
    gs1
  );
  "handle_processes: nothing list" >:: (fun _ -> assert_equal
    (handle_processes [Nothing;Nothing;Nothing;Nothing;Nothing;] gs1 false)
    gs1
  );

  "handle_processes: buy cargo - vehicle" >:: (fun _ -> assert_equal
    gs2_test1.vehicles
    gs2_buycargo1.vehicles
  );
  "handle_processes: buy cargo - player money" >:: (fun _ -> assert_equal
    gs2_test1.players
    gs2_buycargo1.players
  );
  "handle_processes: buy cargo - edge" >:: (fun _ -> assert_equal
    (get_loc 0 gs2_test1.graph)
    (get_loc 0 gs2_buycargo1.graph)
  );

]

let tests = "MTT Test Suite" >::: gamestate_tests
let _ = run_test_tt_main tests