 open Graph
 open GameElements
 open Player
 open InputProcessing
 open DataProcessing

exception EndGame

let buy_vehicle v st =
  let cost = match v.v_t with
    | Car -> car_price
    | Truck -> truck_price in
  let new_players = List.map
    (fun p ->
      if p.p_id = v.v_owner_id
      then if p.money -. cost >= 0.0
           then {p with money = (p.money -. cost)}
           else let () = print_endline "You cannot afford that vehicle." in p
      else p) st.players in
  if new_players = st.players then st else
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
  let route_dest = fun vhcl -> if {vhcl with destination = v.destination; status = v.status} = v
                               then v else vhcl in
  let new_vehicles = List.map route_dest st.vehicles in
  {st with vehicles = new_vehicles;}

let buy_connection c st =
  let loc1 = get_loc c.l_start st.graph in
  let loc2 = get_loc c.l_end st.graph in
  let vertices = Map.fold_vertex
    (fun vx acc -> if vx.l_id = loc1.l_id || vx.l_id = loc2.l_id
                   then vx::acc
                   else acc
    ) st.graph [] in
  let new_graph = match vertices with
    | l1::l2::[] -> Map.add_edge_e st.graph (l1,c,l2)
    | _ -> failwith "Multiple locations with same ids or invalid ids" in
  let cost = road_unit_cost*.(c.length**road_length_cost_exponent) in
  let new_players = List.map
    (fun p ->
      if p.p_id = c.c_owner_id
      then
        (
          if p.money -. cost >= 0.0
           then {p with money = (p.money -. cost)}
           else let () = print_endline ("You cannot afford that road. It costs $"
             ^ (string_of_float (GameGraphics.two_dec cost))) in p)
      else p) st.players in
  if st.players = new_players
  then st
  else {st with graph = new_graph; players = new_players}

let sell_connection c st =
  let loc1 = get_loc c.l_start st.graph in
  let loc2 = get_loc c.l_end st.graph in
  let vertices = Map.fold_vertex
    (fun vx acc -> if vx.l_id = loc1.l_id || vx.l_id = loc2.l_id
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
    (fun vx acc -> if vx.l_id = loc1.l_id || vx.l_id = loc2.l_id
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
    (fun p ->
      if p.p_id = c.c_owner_id
      then if p.money -. cost >= 0.0
           then {p with money = (p.money -. cost)}
           else let () = print_endline ("You cannot afford that road. It costs $"
             ^ (string_of_float (GameGraphics.two_dec cost))) in p
      else p) st.players in
  if st.players = new_players
  then st
  else {st with graph = new_graph; players = new_players}

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
    | None -> failwith "vehicle is not at a valid location"
    | Some l -> l in
  let crgo = match v.cargo with
    | Some c -> c
    | None -> failwith "cannot buy 0 cargo" in
  let t_gp = List.find (fun gp -> gp.resource = crgo.t) buy_location'.produces in
  let cost = t_gp.price *. (float_of_int crgo.quantity) in
  if t_gp.current - crgo.quantity < 0 then st else
  let new_gp = {t_gp with current = t_gp.current - crgo.quantity} in
  let new_gp_list = List.map
    (fun gp -> if gp = t_gp then new_gp else gp) buy_location'.produces in
  let new_location = {buy_location' with produces = new_gp_list} in
  let new_graph = Map.map_vertex
    (fun vx -> if vx.l_id = new_location.l_id
               then new_location
               else vx) st.graph in
  let new_players = List.map
    (fun p ->
      if p.p_id = v.v_owner_id
      then if p.money -. cost >= 0.0
           then {p with money = (p.money -. cost)}
           else p
      else p) st.players in
  if st.players = new_players
  then st
  else {st with vehicles = new_vehicles; players = new_players; graph = new_graph}


let rec handle_processes proclist st road_bought =
  match proclist with
    | [] -> st
    | BuyVehicle(v)::t -> handle_processes t (buy_vehicle v st) road_bought
    | SellVehicle(v)::t -> handle_processes t (sell_vehicle v st) road_bought
    | SetVehicleDestination(v)::t -> handle_processes t (set_v_dest v st) road_bought
    | BuyVehicleCargo(v)::t -> handle_processes t (set_v_cargo v st) road_bought
    | AddRoad(c)::t ->
        handle_processes t (if road_bought then st else buy_connection c st) true
    | DeleteRoad(c)::t -> handle_processes t (sell_connection c st) road_bought
    | PurchaseRoad(c)::t ->
        handle_processes t (if road_bought then st else change_connection_owner c st) true
    | Pause::t-> handle_processes t ({st with paused = not st.paused}) road_bought
    | EndGame::t -> raise EndGame
    | Nothing:: t -> handle_processes t st road_bought

let rec generate_processes st players procs =
  match players with
    | [] -> procs
    | h::t -> if h.p_type = Human
              then generate_processes st t (GameGraphics.click_buttons st h.p_id:: procs)
              else generate_processes st t ((make_c_move st h.p_id) @ procs)

let player_wins st =
  try
    (List.find (fun p -> p.money > win_condition) st.players).p_id
  with
    Not_found -> (-1)

let rec main_loop st =
  try
    let start_t = Unix.time () in
    let p_win = player_wins st in
    if p_win <> (-1) then GameGraphics.draw_winner p_win st else
    GameGraphics.draw_game_state st;
    let processes = generate_processes st st.players [] in
    let st' = handle_processes processes st false in
    let new_vehicles = update_vehicles st'.vehicles st'.graph st'.players st' in
    let new_graph = update_locations st'.graph st'.game_age in
    let st'' =
      {
        vehicles = new_vehicles;
        graph = new_graph;
        players = st'.players;
        paused = st'.paused;
        game_age = st'.game_age + 1;
        ai_info = st.ai_info
      }
    in
    let time_elapsed = Unix.time () -. start_t in
    (* print_endline (string_of_float time_elapsed); *)
    let sleep_time = if ((1.0 /. fps) -. time_elapsed) > 0.0 then ((1.0 /. fps) -. time_elapsed) else 0.0 in
    (* print_endline (string_of_float sleep_time); *)
    Unix.sleepf sleep_time;
    main_loop st'';
  with
     | EndGame | Graphics.Graphic_failure _ ->
        print_endline "Game Over, autosaving to data/autosave.json";
        DataProcessing.save_file st "data/autosave.json"
     | _ ->
        print_endline "Unexpected error, attempting save to data/failsave.json";
        DataProcessing.save_file st "data/failsave.json"

let round flt =
  int_of_float (flt +. 0.5)

let two_dec flt =
  float_of_int (round (flt *. 100.)) /. 100.

let rec init_game fname opt =
  try
    Graphics.resize_window 1000 600;
    let start_t = Unix.time () in
    let init_gs = DataProcessing.load_file fname in
    main_loop init_gs;
    gameover ();
    print_endline ("\nGame Duration: " ^
      (string_of_float (two_dec(Unix.time () -. start_t)/.60.)) ^ " minutes.");
    Unix.sleepf 1.;
    title_screen ()
  with
  | Failure _ -> print_endline "\nNot a valid game file"; title_screen ()
  | e -> raise EndGame

and title_screen () =
  try
    GameGraphics.draw_start ();
    let opt = GameGraphics.title_click () in
    let file_name = if opt = 1 then "data/game.json" else if opt = 2 then (
    print_endline "\nPlease enter the name of the game file you want to load.\n";
    print_string  "> "; read_line () ) else failwith "Not an option yet" in
    init_game file_name opt
  with
     | EndGame | Graphics.Graphic_failure _ ->
        print_endline "\nGoodbye"
     | _ ->
        print_endline ("Unexpected error, attempting save to data/failsave.json");
        print_endline "\nBye"

and gameover () = 
  print_endline
  "\n#######################################################################";
  print_endline
  "                                Game Over                                ";
  print_endline
  "#######################################################################"

let instr () =
   print_string
    "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n";
  print_endline
    "***********************************************************************";
  print_endline "                             Instructions";
  print_endline
    "***********************************************************************\n";
  print_endline "Save/Quit: Saves the current game to a json file and closes the game.\n";
  print_endline "Pause:     Pauses the game until the screen is clicked again\n";
  print_endline "Buy Car:   Buys a car starting at a given location\n";
  print_endline "Buy Truck: Buys a truck starting at a given location\n";
  print_endline "Buy Road:  Buys a new road between two locations, or if a road exists,";
  print_endline "           buys exclusive right to that road\n";
  print_endline
    "***********************************************************************\n"
