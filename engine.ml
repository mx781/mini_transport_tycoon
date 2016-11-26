 open Graph
 open GameElements
 open Player
 open InputProcessing

let prob = 0.0
let new_graph () =
  let v1 = {l_id = 0;
  l_x = 450.0;
  l_y = 430.0;
  accepts= [
  {
  resource= Oil;
  steps_to_inc= 50; (*how many game_steps before incrementing current by 1*)
  current= 20;
  capacity= 40;
  price= 1.4;
  natural_price= 1.4;
}
];
  produces= [{
  resource= Oil;
  steps_to_inc= 100; (*how many game_steps before incrementing current by 1*)
  current= 100;
  capacity= 140;
  price= 3.4;
  natural_price= 4.4;
}];} in
  let v2 = {l_id = 1;
  l_x = 150.0;
  l_y = 300.0;
  accepts= [];
  produces= [];} in
  let v3 = {l_id = 2;
  l_x = 430.0;
  l_y = 200.0;
  accepts= [];
  produces= [];} in
  let m1 = Map.add_vertex (Map.empty) (v1) in
  let m2 = Map.add_vertex m1 (v2) in
  let m3 = Map.add_edge_e m2
  (v1,{
    c_owner_id = 4;
    l_start= 1;
    l_end =  2;
    length= 2.0;
    c_age= 0;
    c_speed= 3.0;
  },v2) in
  let m4 = Map.add_vertex m3 (v3) in
 let m5 = Map.add_edge_e m4
  (v1,{
    c_owner_id = 4;
    l_start= 1;
    l_end =  2;
    length= 2.0;
    c_age= 0;
    c_speed= 3.0;

  },v3) in m5

let fps = 30.0
let car_price = 100.0
let truck_price = 200.0
let sell_back_percentage = 0.6

let buy_vehicle v st =
  let cost = match v.v_t with
    | Car -> car_price
    | Truck -> truck_price in
  let new_players = List.map
    (fun p -> if p.p_id = v.v_owner_id
              then {p with money = (p.money -. cost)}
              else p) st.players in
  {st with vehicles = v::st.vehicles; players = new_players}

let sell_vehicle v st =
  let new_vehicles = List.filter (fun vhcl -> vhcl <> v) st.vehicles in
  let gain = match v.v_t with
    | Car -> car_price*.sell_back_percentage
    | Truck -> truck_price*.sell_back_percentage in
  let new_players = List.map
    (fun p -> if p.p_id = v.v_owner_id
              then {p with money = (p.money +. gain)}
              else p) st.players in
    {st with vehicles = new_vehicles; players = new_players}

let rec handle_processes proclist st =
  match proclist with
    | [] -> st
    | BuyVehicle(v)::t -> buy_vehicle v st
    | SellVehicle(v)::t -> sell_vehicle v st

let rec main_loop st =
  try
    let start_t = Sys.time () in
    GameGraphics.draw_game_state st;
    let processes = [] in
    let st' = handle_processes processes st in
    let new_vehicles = update_vehicles st'.vehicles st'.graph st'.players st' in
    let new_graph = update_locations st'.graph st'.game_age in
    let st'' = { vehicles = new_vehicles;
                graph = new_graph;
                players = st'.players;
                paused = st'.paused;
                game_age = st'.game_age + 1;} in
    let time_elapsed = Sys.time () -. start_t in
    (* print_endline (string_of_float time_elapsed); *)
    let sleep_time = if ((1.0 /. fps) -. time_elapsed) > 0.0 then ((1.0 /. fps) -. time_elapsed) else 0.0 in
    (* print_endline (string_of_float sleep_time); *)
    Unix.sleepf sleep_time;
    main_loop st''
  with _ -> print_endline
  "###########################################################################";
  print_endline
  "##                             Game Over                                 ##";
  print_endline
  "###########################################################################"

let init_game fname scale =
  GameGraphics.open_screen scale;
  main_loop
  { vehicles=
  [{v_loc = None;
  v_owner_id= 0;
  v_t = Car;
  speed = 1.0;
  capacity= 100;
  cargo= Some {
  t = Oil;
  quantity = 5;
  };
  age= 56;
  status= Driving;
  x= 224.0;
  y= 122.0;
  destination= [1;0;2;0];};
  {v_loc = None;
  v_owner_id= 0;
  v_t = Truck;
  speed = 5.0;
  capacity= 100;
  cargo= Some {
  t = Oil;
  quantity = 5;
  };
  age= 5;
  status= Driving;
  x= 24.0;
  y= 302.0;
  destination= [2;1;0];};
  {v_loc = None;
  v_owner_id= 1;
  v_t = Truck;
  speed = 1.4;
  capacity= 100;
  cargo= Some {
  t = Oil;
  quantity = 5;
  };
  age= 5;
  status= Driving;
  x= 450.0;
  y= 12.0;
  destination= [0;1;2;0];}];
  graph = new_graph () ;

  players = [
    {p_id = 0; p_type = Human; money = 100.0};
    {p_id = 1; p_type = AI(2); money = 200.0};
  ];
    game_age = 0; paused = false;}
