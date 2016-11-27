 open Graph
 open GameElements
 open Player
 open InputProcessing
 open DataProcessing

let fps = 24.0
let car_price = 100.0
let truck_price = 200.0
let sell_back_percentage = 0.6
let road_unit_cost = 1.0
let road_length_cost_exponent = 1.2
let road_rights_unit_cost = 0.4
exception EndGame

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

let set_v_dest v st =
  let route_dest = fun vhcl -> if {vhcl with destination = v.destination} = v
                               then v else vhcl in
  let new_vehicles = List.map route_dest st.vehicles in
  {st with vehicles = new_vehicles}

let buy_connection c st =
  let loc1 = get_loc c.l_start st.graph in
  let loc2 = get_loc c.l_end st.graph in
  let vertices = Map.fold_vertex
    (fun vx acc -> if vx.l_id = loc1.l_id || vx.l_id = loc1.l_id
                   then vx::acc
                   else acc
    ) st.graph [] in
  let new_graph = match vertices with
    | l1::l2::[] -> Map.add_edge_e st.graph (l1,c,l2)
    | _ -> failwith "Multiple locations with same ids or invalid ids" in
  let cost = road_unit_cost*.(c.length**road_length_cost_exponent) in
  let new_players = List.map
    (fun p -> if p.p_id = c.c_owner_id
              then {p with money = (p.money -. cost)}
              else p) st.players in
  {st with graph = new_graph; players = new_players}

let sell_connection c st =
  let loc1 = get_loc c.l_start st.graph in
  let loc2 = get_loc c.l_end st.graph in
  let vertices = Map.fold_vertex
    (fun vx acc -> if vx.l_id = loc1.l_id || vx.l_id = loc1.l_id
                   then vx::acc
                   else acc
    ) st.graph [] in
  let new_graph = match vertices with
    | l1::l2::[] -> Map.remove_edge_e st.graph (Map.find_edge st.graph l1 l2)
    | _ -> failwith "Multiple locations with same ids or invalid ids" in
  let gain = (road_unit_cost*.(c.length**road_length_cost_exponent))
    *.sell_back_percentage in
  let new_players = List.map
    (fun p -> if p.p_id = c.c_owner_id
              then {p with money = (p.money +. gain)}
              else p) st.players in
  {st with graph = new_graph; players = new_players}

let change_connection_owner c st =
  let loc1 = get_loc c.l_start st.graph in
  let loc2 = get_loc c.l_end st.graph in
  let vertices = Map.fold_vertex
    (fun vx acc -> if vx.l_id = loc1.l_id || vx.l_id = loc1.l_id
                   then vx::acc
                   else acc
    ) st.graph [] in
  let new_graph = match vertices with
    | l1::l2::[] ->
      let g' = Map.remove_edge_e st.graph (Map.find_edge st.graph l1 l2) in
      Map.add_edge_e g' (l1,c,l2)
    | _ -> failwith "Multiple locations with same ids or invalid ids" in
  let cost = road_rights_unit_cost*.(c.length) in
  let new_players = List.map
    (fun p -> if p.p_id = c.c_owner_id
              then {p with money = (p.money -. cost)}
              else p) st.players in
  {st with graph = new_graph; players = new_players}

let set_v_cargo v st =
  let swap_vehicle = fun vhcl -> if {vhcl with cargo = v.cargo} = v
                               then v else vhcl in
  let new_vehicles = List.map swap_vehicle st.vehicles in
  let buy_location = Map.fold_vertex (fun vx acc ->
    if vx.l_x = v.x && vx.l_y = v.y
    then if acc = None
         then Some vx
         else None
    else acc) st.graph None in
  let buy_location' = match buy_location with
    | None -> failwith"vehicle is not at a valid location"
    | Some l -> l in
  let crgo = match v.cargo with
    | Some c -> c
    | None -> failwith "can not buy 0 cargo" in
  let t_gp = List.find (fun gp -> gp.resource = crgo.t) buy_location'.produces in
  let cost = t_gp.price *. (float_of_int crgo.quantity) in
  let new_gp = {t_gp with current = t_gp.current - crgo.quantity} in
  let new_gp_list = List.map
    (fun gp -> if gp = t_gp then new_gp else gp) buy_location'.produces in
  let new_location = {buy_location' with produces = new_gp_list} in
  let new_graph = Map.map_vertex
    (fun vx -> if vx.l_id = new_location.l_id
               then new_location
               else vx) st.graph in
  let new_players = List.map
    (fun p -> if p.p_id = v.v_owner_id
              then {p with money = (p.money -. cost)}
              else p) st.players in
  {st with vehicles = new_vehicles; players = new_players; graph = new_graph}


let rec handle_processes proclist st =
  match proclist with
    | [] -> st
    | BuyVehicle(v)::t -> handle_processes t (buy_vehicle v st)
    | SellVehicle(v)::t -> handle_processes t (sell_vehicle v st)
    | SetVehicleDestination(v)::t -> handle_processes t (set_v_dest v st)
    | BuyVehicleCargo(v)::t -> handle_processes t (set_v_cargo v st)
    | AddRoad(c)::t -> handle_processes t (buy_connection c st)
    | DeleteRoad(c)::t -> handle_processes t (sell_connection c st)
    | PurchaseRoad(c)::t -> handle_processes t (change_connection_owner c st)
    | Pause::t-> handle_processes t ({st with paused = not st.paused})
    | EndGame::t -> raise EndGame
    | Nothing:: t -> handle_processes t st

let rec generate_processes st players procs =
  match players with
    | [] -> procs
    | h::t -> if h.p_type = Human
              then generate_processes st t procs
              else generate_processes st t ((make_c_move st h.p_id) @ procs)

let rec main_loop st =
  try
    let start_t = Sys.time () in
    GameGraphics.draw_game_state st;
    let processes = generate_processes st st.players [] in
    let st' = handle_processes processes st in
    let new_vehicles = update_vehicles st'.vehicles st'.graph st'.players st' in
    let new_graph = update_locations st'.graph st'.game_age in
    let st'' = { vehicles = new_vehicles;
                graph = new_graph;
                players = st'.players;
                paused = st'.paused;
                game_age = st'.game_age + 1;
                ai_info = st.ai_info} in
    let time_elapsed = Sys.time () -. start_t in
    (* print_endline (string_of_float time_elapsed); *)
    let sleep_time = if ((1.0 /. fps) -. time_elapsed) > 0.0 then ((1.0 /. fps) -. time_elapsed) else 0.0 in
    (* print_endline (string_of_float sleep_time); *)
    Unix.sleepf sleep_time;
    main_loop st''
  with e -> print_endline
  "\n#########################################################################";
  print_endline
  "                               Game Over                               ";
  print_endline
  "#########################################################################"

let init_game fname scale =
  GameGraphics.open_screen scale;
  let init_gs = DataProcessing.load_file fname in
  main_loop init_gs;
  DataProcessing.save_file init_gs "data/gamesave.json"
