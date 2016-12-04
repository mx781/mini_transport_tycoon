 open Graph
 open GameElements
 open Player
 open InputProcessing
 open DataProcessing

exception EndGame
exception Quit

(* pre: v is a valid vehicle record (fields can be of any value)
 *      st is a valid game state, that is, there are no duplicate
 *      location or player ids in the player list or graph, and there is a
 *      player id corresponding to the v_owner_id of v in st.
 * post: [buy_vehicle v st] returns an updated game state with the vehicle
 *       purchased iff the player corresponding to the the v_owner_id of v
 *       can afford the vehicle.
 *)
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

(* pre: v is a valid vehicle record that is contained in the vehicle list of st
 *      st is a valid game state, that is, there are no duplicate
 *      location or player ids in the player list or graph, and there is a
 *      player id corresponding to the v_owner_id of v in st.
 * post: [sell_vehicle v st] returns an updated game state with the vehicle
 *       sold.
 *)
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

(* pre: v is a valid vehicle record that is contained in the vehicle list of st
 *      with the exception that the vehicle status and previous destination may
 *      be changed.
 *      st is a valid game state, that is, there are no duplicate
 *      location or player ids in the player list or graph.
 * post: [set_v_dest v st] returns an updated game state with the vehicle's
 *       destination set.
 *)
let set_v_dest v st =
  let route_dest = fun vhcl ->
    if {vhcl with destination = v.destination; status = v.status} = v
    then v
    else vhcl in
  let new_vehicles = List.map route_dest st.vehicles in
  {st with vehicles = new_vehicles;}

(* pre: c is a valid connection whose two location ids correspond to two
 *      locations that are present in the graph of game state st and there is
 *      not already a connection of any player directly between these two
 *      locations
 *      st is a valid game state, that is, there are no duplicate
 *      location or player ids in the player list or graph, and there is a
 *      player id corresponding to the c_owner_id of c in st.
 * post: [buy_connection c st] returns an updated game state with the connection
 *       purchased iff the player corresponding to the the c_owner_id of c
 *       can afford the connection.
 *)
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
           else let () = print_endline ("You cannot afford that road. It costs"
             ^ " $" ^ (string_of_float (GameGraphics.two_dec cost))) in p)
      else p) st.players in
  if st.players = new_players
  then st
  else {st with graph = new_graph; players = new_players}

(* pre: c is a valid connection whose two location ids correspond to two
 *      locations that are present in the graph of game state st and there is
 *      already a connection of the player corresponding to the c_owner_id of c
 *      in the graph of st.
 *      st is a valid game state, that is, there are no duplicate
 *      location or player ids in the player list or graph
 * post: [sell_connection c st] returns an updated game state with the
 *       connection sold.
 *)
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

(* pre: c is a valid connection whose two location ids correspond to two
 *      locations that are present in the graph of game state st and there is
 *      already a public connection between these two locations. Here "public"
 *      means that the c_owner_id of the existing road is -1.
 *      st is a valid game state, that is, there are no duplicate
 *      location or player ids in the player list or graph, and there is a
 *      player id corresponding to the c_owner_id of c in st.
 * post: [change_connection_owner c st] returns an updated game state with the
 *       connection owener changed iff the player corresponding to the the
 *       c_owner_id of c can afford the connection transfer.
 *)
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
           else let () = print_endline ("You cannot afford that road. It costs"
             ^ " $" ^ (string_of_float (GameGraphics.two_dec cost))) in p
      else p) st.players in
  if st.players = new_players
  then st
  else {st with graph = new_graph; players = new_players}

(* pre: v is a valid vehicle that is present in the vehicle list of game state
 *      st that is waiting at any location with the exception that the cargo
 *      field of v has been changed.
 *      st is a valid game state, that is, there are no duplicate
 *      location or player ids in the player list or graph.
 * post: [set_v_cargo v st] returns an updated game state with the cargo
 *       purchased and stored in the vehicle iff the player corresponding to
 *       the v_owner_id of v can afford the cargo based on the current market
 *       price.
 *)
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
  let get_cargo_type = (fun gp -> gp.resource = crgo.t) in
  let t_gp = List.find get_cargo_type buy_location'.produces in
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
  else
    {st with vehicles = new_vehicles; players = new_players; graph = new_graph}

(* pre: proclist is any list of valid processes (that is, they must be of the
 *      format such that they are actionable based on the current game state st.
 *      Actionable is defined here as conforming to the rules in the formal
 *      instructions provided in the separate instructions document.
 *      st is a valid game state, that is, there are no duplicate
 *      location or player ids in the player list or graph.
 *      road_bought is true if a road has been bought in this frame by any
 *      player in this frame of the game and false otherwise.
 * post: [handle_processes proclist st road_bought] returns a new game state
 *       updated to reflect all changes that these processes, once handled, will
 *       cause in the game state. However, it will only allow one road to be
 *       purchased for frame to avoid same-frame action conflicts.
 *)
let rec handle_processes proclist st road_bought =
  match proclist with
    | [] -> st
    | BuyVehicle(v)::t -> handle_processes t (buy_vehicle v st) road_bought
    | SellVehicle(v)::t -> handle_processes t (sell_vehicle v st) road_bought
    | SetVehicleDestination(v)::t ->
        handle_processes t (set_v_dest v st) road_bought
    | BuyVehicleCargo(v)::t -> handle_processes t (set_v_cargo v st) road_bought
    | AddRoad(c)::t ->
        handle_processes t
          (if road_bought then st else buy_connection c st) true
    | DeleteRoad(c)::t -> handle_processes t (sell_connection c st) road_bought
    | PurchaseRoad(c)::t ->
        handle_processes t
          (if road_bought then st else change_connection_owner c st) true
    | Pause::t->
        handle_processes t ({st with paused = not st.paused}) road_bought
    | EndGame::t -> raise EndGame
    | Nothing::t -> handle_processes t st road_bought

(* pre: st is a valid game state, that is, there are no duplicate
 *      location or player ids in the player list or graph.
 *      players is a list of player records.
 *      procs is a list of valid processes.
 * post: [generate_processes st players procs] returns a list of valid processes
 *       generated in this frame of the game execution, including both those
 *       generated from human input in the window and those that are created by
 *       the AIs.
 *)
let rec generate_processes st players procs =
  match players with
    | [] -> procs
    | h::t -> if h.p_type = Human
              then generate_processes st t
                (GameGraphics.click_buttons st h.p_id:: procs)
              else generate_processes st t ((make_c_move st h.p_id) @ procs)

(* pre: st is a valid game state, that is, there are no duplicate
 *      location or player ids in the player list or graph.
 * post: [player_wins st] is the player_id of the player who won the game if
 *       a player has reached the win_condition threshold, and it is -1
 *       otherwise
 *)
let player_wins st =
  try
    (List.find (fun p -> p.money > win_condition) st.players).p_id
  with
    Not_found -> (-1)

(* pre: st is a valid game state, that is, there are no duplicate
 *      location or player ids in the player list or graph.
 * post: [main_loop st] will return the game state of the game when it is
 *       eventually exited. It is our main repl loop and ordinarily recursively
 *       calls itself to calculate and update each frame.
 *)
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
      }
    in
    let time_elapsed = Unix.time () -. start_t in
    let sleep_time = if ((1.0 /. fps) -. time_elapsed) > 0.0
                     then ((1.0 /. fps) -. time_elapsed) else 0.0 in
    Unix.sleepf sleep_time;
    main_loop st''
  with
     | EndGame | Graphics.Graphic_failure _ ->
        print_endline "Game Over, autosaving to data/autosave.json";
        DataProcessing.save_file st "data/autosave.json"
     | Quit -> ()
     | _ ->
        print_endline "Unexpected error, attempting save to data/failsave.json";
        DataProcessing.save_file st "data/failsave.json"

(*[round flt] rounds a float flt to the nearest integer.*)
let round flt =
  int_of_float (flt +. 0.5)

(*[two_dec flt] trims all digits after the second decimal place of a float flt*)
let two_dec flt =
  float_of_int (round (flt *. 100.)) /. 100.

(*[inst ()] prints out some quick help instructions for the player.*)
let instr () =
   print_string
    "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n";
  print_endline
    "***********************************************************************";
  print_endline "                             Instructions";
  print_endline
    "***********************************************************************\n";
  print_endline ("Save/Quit:  Saves the current game to a json file and" ^
    " closes the game.\n");
  print_endline ("Pause:      Pauses the game until the screen is" ^
    " clicked again\n");
  print_endline "Buy Car:    Buys a car starting at a given location\n";
  print_endline "Buy Truck:  Buys a truck starting at a given location\n";
  print_endline ("Buy Road:   Buys a new road between two locations, " ^
    "or if a road exists,");
  print_endline "            buys exclusive right to that road\n";
  print_endline "Sell Road:  Sells an existing road between two locations if";
  print_endline "            it exists and you own it\n";
  print_endline "Sell Auto:  Sells an existing vehicle that you own\n";
  print_endline "Add Cargo:  Buys cargo for a specific vehicle at a location\n";
  print_endline "Confirm:    Confirm a selection (road purchase or sale) \n";
  print_endline "Cancel:     Cancel a selection (of any above action) \n";
  print_endline
    "***********************************************************************\n"

(* pre: fname is a file name (which need not be valid but must reflect an actual
 *      game json file is the file is to be loaded)
 *      dif is the difficulty of the AIs (Easy, Medium, Hard, or Brutal).
 * post: [init_game fname dif] returns a unit but starts the main loop of the
 *       game and sets up a game state, either from loading a file or starting
 *       a new game*)
let rec init_game fname dif : unit =
  try
    Graphics.resize_window 1000 600;
    let start_t = Unix.time () in
    let init_gs = DataProcessing.load_file fname in
    let init_gs' = set_game_difficulty dif init_gs in
    print_endline "Start transporting!\n";
    main_loop init_gs';
    gameover ();
    print_endline ("\nGame Duration: " ^
      (string_of_float (two_dec(Unix.time () -. start_t)/.fps))^" minutes.\n");
    Unix.sleepf 0.5;
    title_screen dif
  with
  | Quit -> raise Quit
  | Failure e -> print_endline e;
                 print_endline ("\nNot a valid game file.  "^
                               "Load a different file or start a new game.\n");
                 title_screen dif
  | e -> raise EndGame

(* pre: dif is an AI difficulty (Easy, Medium, Hard, or Brutal)
 * post: [title_screen dif] displays the title screen and just returns a unit.
 *)
and title_screen (dif:ai_game_difficulty) : unit =
  try
    GameGraphics.draw_start ();
    let opt = GameGraphics.title_click () in
    if opt = 1 then init_game "data/game.json" dif
    else if opt = 2 then (
      print_endline
        "\nPlease enter the name of the game file you want to load.";
      print_endline "(Saved games are stored in data/save.json)\n";
      print_string  "> "; init_game (read_line ()) dif )
    else if opt = 3 then (instr (); title_screen dif)
    else if opt = 4 then (settings_screen ())
    else if opt = 5 then raise Quit
    else raise Quit
  with
  | Failure _ -> title_screen dif
  | _ -> ()


(* [settings_screen ()] just displays the settings screen and returns a unit.
 *)
and settings_screen () =
    title_screen (GameGraphics.settings ())

(*[game over ()] just displays game over text in the terminal.*)
and gameover () =
  print_endline
  "\n#######################################################################";
  print_endline
  "                                Game Over                                ";
  print_endline
  "#######################################################################"
