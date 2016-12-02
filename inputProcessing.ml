open GameElements
open Player
type process =
  | BuyVehicle of vehicle
  | SellVehicle of vehicle
  | SetVehicleDestination of vehicle (*with updated destinations*)
  | BuyVehicleCargo of vehicle
  | AddRoad of connection
  | DeleteRoad of connection
  | PurchaseRoad of connection (*Purchase rights to a road that is preexisting*)
  | Pause
  | EndGame
  | Nothing


(*Gets a player from the player list*)
let rec get_player p_id (p_list:Player.player list)=
  match p_list with
  |h :: t-> if h.p_id = p_id then h else get_player p_id t
  |[]-> failwith "trying to get player when non-existent, TALK TO DAN"

(*Gets owned vehicles from a player as a list*)
let rec get_owned_vehicles p_id v_list =
  List.fold_left(fun x y -> if y.v_owner_id = p_id then y :: x else x) [] v_list

(*Getting things out of tuples*)
let f (x, _, _) = x
let s (_,y,_) = y
let t (_,_,z) = z

(*Gets roads accessible from player as a list*)
let get_roads p_id graph=
  Map.fold_edges_e
    (fun x y->if (Map.E.label x).c_owner_id=p_id ||
      (Map.E.label x).c_owner_id = (-1) then x::y else y) graph []

(*Gets computer player's roads*)
let get_c_roads p_id graph =
Map.fold_edges_e
    (fun x y->if (Map.E.label x).c_owner_id=p_id then x::y else y) graph []

(*Allows the AI to activate waiting vehicles*)
let rec activate_vehicles v_list =
  match v_list with
  |h :: t ->  if h.status = Waiting && h. cargo = None then Some h
              else activate_vehicles t
  |[] -> None

(*Gets details about location from graph.*)
let get_loc_details graph loc_id =
  let loc_info =
    Map.fold_vertex
    (fun x y -> if (Map.V.label x).l_id = loc_id then x::y else y) graph [] in
  match loc_info with
  |[h] -> h
  |_ -> failwith "Trying to get info about more than one loc"

(*Gets info about a player from an int id*)
let rec get_p_info id players =
  match players with
  |h :: t -> if h.p_id = id then h else get_p_info id t
  |[] -> failwith "cannot find player ID, TALK TO DAN"

(*Adds edges to graph*)
let rec add_edges graph edge_list =
  match edge_list with
  |[] -> graph
  |h :: t -> add_edges (Map.add_edge_e graph h) t

(*Sorts items from cheapest to most expensive.*)
let sort_items e1 e2 =
  if e1.price > e2.price then 1 else if e1.price = e2.price then 0 else (-1)

(*Get profit from good in particular location*)
let rec good_cost (goods_list:goods_profile list) (good:goods_profile)=
  match goods_list with
  |h :: t ->
    if h.resource = good.resource then (h.price -. good.price)
    else good_cost t good
  |[] -> 0.

(*Get maximum PROFIT from an item in graph accessible from connections*)
(*Returns the profit, the location to sell the good to, and the good itself *)
let get_good_profit graph good curr_loc =
  Map.fold_vertex
  (fun x y -> let profit = good_cost x.accepts good in
    let new_path  =
      try
        match Dijkstra.shortest_path graph x curr_loc with
        |_ -> true
      with
        |Not_found -> false in
    if new_path && profit > f y && good.current > min_bought
    then (profit, Some x, Some good) else y) graph
  (0., None, None)

(*See if item exists in any particular location that can be connected
 *through Dijkstra's. Find the one with the maximum price difference.
 *Produces a triple with the profit, the location to sell the good to, and the
 * good itself*)
let rec get_good_loc graph goods_produced curr_m curr_loc =
  List.fold_left
  (fun x y->
    let profits = get_good_profit graph y curr_loc in
    let enough_money = curr_m >= y.price in
    if enough_money &&  f profits > f x then profits else x)
  (0., None, None) goods_produced

(*Gets stuff out of options*)
let get_o op =
  match op with
  |Some x -> x
  |None -> failwith "trying to get option out of nothing, TALK TO DAN"

(*Sees if it is possible to get from loc1 to loc2 for player with p_id*)
let get_route (loc1:int) (loc2:int) state (p_id:int)=
  let accessible_roads = get_roads p_id state.graph in
  let empty = Map.empty in
  let new_graph = add_edges empty accessible_roads in
  let loc_info1 = get_loc_details state.graph loc1 in
  let loc_info2 = get_loc_details state.graph loc2 in
  let new_path =
    (try Dijkstra.shortest_path new_graph loc_info1 loc_info2 with
    |Not_found ->([], (-1.))) in
  if new_path = ([],(-1.)) then
    None
  else
    Some (List.map (fun x -> (t x).l_id) (fst new_path))


(*Gets the maximum quantity that can taken by a vehicle*)
let get_quantity vehicle r_type location curr_money=
  let vehicle_max = match vehicle.v_t with
    | Car -> car_capacity
    | Truck -> truck_capacity in
  let accepts = List.find (fun gp -> gp.resource = r_type) location.produces in
  let maxq = min accepts.current vehicle_max in
  let maxq'= min maxq (int_of_float (curr_money /. accepts.price)) in
    maxq'

(*Gets a new location close to the vehicle. First checks if other locations are accessible.
 *The second is the result of the movement.*)
let get_new_dest graph_access curr_loc =
  Map.fold_edges_e (fun x y -> let () = Random.self_init() in
    let r = Random.int (2) in
    if r = 0 || y = None then
      (if (f x).l_id = curr_loc.l_id then Some (t x)
      else if (t x).l_id = curr_loc.l_id then Some (f x)
      else y)
    else y) graph_access None

(*Gets the number of locations connected to loc1 and loc2; ideally used to
 *determine if an AI is buying a road that only connects two locations...*)
let get_graph_islands graph loc1 loc2 = failwith "unimplemented"

(*Stuff that chooses AI movement based on a list of connections*)
(*TODO: Fix quantity bought.*)
let make_vehicle_move vehicle c_connections graph curr_m (ai_info:ai_stuff) c_id =
  (*Create a new graph with only these connections.*)
  (* let debug = if vehicle.cargo = None then true else failwith "vehicle not empty" in *)
  let empty = Map.empty in
  let new_graph = add_edges empty c_connections in
  let cur_loc =
    match vehicle.v_loc with
    |None -> failwith "Passive vehicle with no location, talk to Dan"
    |Some loc -> loc in
  (* print_endline (string_of_int cur_loc); *)
  let loc_details = get_loc_details graph cur_loc in
  let goods = loc_details.produces in
  let max_profits = get_good_loc new_graph goods curr_m loc_details in
  (*PROFITS FROM CURRENT LOCATION*)
  if f max_profits > 0. then
    (* print_endline "asdf2"; *)
    let the_good = (get_o (t max_profits)).resource in
    let q = get_quantity vehicle the_good loc_details curr_m in
    if q > 0 then
      (
      let new_path =
        try Dijkstra.shortest_path new_graph loc_details (get_o (s max_profits)) with
        |Not_found -> failwith "trying to get path that doesn't exist??, TALK TO DAN" in
      let destinations = List.map (fun x -> (t x).l_id) (fst new_path) in
      let new_cargo_vehicle =
        {vehicle with cargo = Some ({t = the_good; quantity = q})} in
      let new_dest_vehicle =
        {new_cargo_vehicle with destination = destinations; status = Driving} in
      (* print_endline (string_of_int (List.hd destinations) ^ "halp"); *)
      [BuyVehicleCargo new_cargo_vehicle;SetVehicleDestination new_dest_vehicle])
    else []
  else
    ((* print_endline "TEST@"; *)
    match get_new_dest new_graph loc_details with
    (*Vehicle is stuck. Buy a road if possible or sell if not.*)
    |None -> [SellVehicle vehicle]
    (*Choosing not to move due to randomness*)
    |Some loc1 ->
      [SetVehicleDestination
      ({vehicle with destination = [loc1.l_id]; status = Driving})])

(*Gets total capacity from a list*)
let rec get_vehicle_capacity v_list total=
  match v_list with
  |h :: t -> get_vehicle_capacity t (total + h.capacity)
  |[] -> total

(*Get good cost at a particular location. None if the good is not sold there.*)
let rec get_good_cost goods_list good =
  match goods_list with
  |h :: t -> if h.resource = good then Some h.price else get_good_cost t good
  |[] -> None

(*Iterates over all the edges to determine whether a road exists THAT IS NOT
 *PUBLIC*)
let edge_exists graph initial_loc final_loc =
  Map.fold_edges_e (fun x y -> if y = true || ((f x).l_id = initial_loc.l_id &&
    (t x).l_id = final_loc.l_id || (f x).l_id = final_loc.l_id &&
    (t x).l_id = initial_loc.l_id)  && (s x).c_owner_id <> (-1)  then (true) else false
  ) graph false

(*Does the same as edge_exists, but includes public roads*)

(*Returns the price, location option, and good option describing the location with
 *a good at the cheapest and most expensive prices (produces/accepts).
 *CHEAPEST FIRST, MOST EXPENSIVE SECOND*)
let good_loc_info graph good money=
  Map.fold_vertex (fun x y ->
    let cheaper =
    match get_good_cost x.produces good with
    |None -> fst y
    |Some price ->
      if s (snd y) <> None then
        (*Get price of road*)
        let road_cost = calculate_buy_road_cost x (get_o (s (snd y))) graph in
        (if price < f (fst y)&& not(edge_exists graph x (get_o (s (snd y))))
          && road_cost +. safe_amount +. truck_price <= money then
          (price, Some x, Some good)
        else
          fst y)
      else
        (if price < f (fst y) then (price, Some x, Some good) else fst y) in
    let expensive =
    match get_good_cost x.accepts good with
    |None -> snd y
    |Some price ->
      if s (fst y) <> None then
        let road_cost = calculate_buy_road_cost x (get_o (s (fst y))) graph in
        (if price > f (snd y)&& not(edge_exists graph x (get_o (s (fst y))))
          && road_cost +. safe_amount +. truck_price <= money then
          (price, Some x, Some good)
        else
          snd y)
      else
        (if price > f (snd y) then (price, Some x, Some good) else snd y) in
      (*TODO: DEBUG*)
      (cheaper,expensive)
    )
  graph ((large_float, None, None),(small_float, None, None))

(*Returns the price, location option, and good option containing the cheapest and
 *most expensive goods.
 *Used for profit calculations.*)
let get_good_diffs graph goods c_money=
  List.map (fun x -> good_loc_info graph x c_money) goods

(*Gets one of the greatest diffs from from the result in get_good_locs*)
(*TODO: Fix manner of getting greatest diffs. RIGHT NOW IT'S A HACK*)
let get_greatest_dif good_dif graph =
  List.fold_left (fun x y -> let new_diff = (f (snd y) -. f (fst y)) in
    let old_diff = (f (snd x) -. f (fst x)) in
    let has_been_built =
      if s (snd y) <> None && s (fst y) <> None then
        edge_exists graph (get_o (s (snd y))) (get_o (s (fst y)))
      else true in
    if old_diff < new_diff && (not has_been_built) then y else x)
    ((large_float, None, None),(small_float, None, None)) good_dif

(*Returns an end_loc based on income. Also checks if the road has already been built*)
 let rec build_road graph location good price (ai_info:ai_stuff) c_info loc2=
  (* print_endline ("TESTF"); *)
  Map.fold_vertex (fun x y ->
    let good_price =
      if get_good_cost x.accepts good = None then (0.) else ((* print_endline "asdfasdf"; *)
      get_o (get_good_cost x.accepts good)) in
    let length = hypot (location.l_x-.x.l_x) (location.l_y-. x.l_y) in
    (*Check if road can be bought*)
    if road_unit_cost*.(length**road_length_cost_exponent) +. car_price <=
      c_info.money
    then
     ( print_endline "asdf4";
      Some x)
    else
     ( (* print_endline (string_of_float (f (get_o ((get_o (!ai_info)) (c_info.p_id, None))) +. 0.000001));
       print_endline (string_of_float (good_price -. price)^"ONE");
      print_endline (string_of_float (road_unit_cost*.(length**road_length_cost_exponent) +. car_price)); *)
      y)
    ) graph None

(*SOME INEFFICENCIES INVOLVING AI WILL BE FIXED LATER : TODO *)
(*Determines what road to buy. Uses information based on the greatest profit.*)
let buy_c_road graph (ai_info:ai_stuff) c_info =
  let goods = [Lumber; Iron; Oil; Electronics; Produce] in
  (*Gets cheapest out of all locations*)
  let price_difs = get_good_diffs graph goods c_info.money in
  let most_profitable = get_greatest_dif price_difs graph in
  if s (fst most_profitable) = None || s (snd most_profitable) = None
    || f (snd most_profitable) -. f (fst most_profitable) <= min_profit then
    None
  else
    let loc1 = get_o (s (fst most_profitable)) in
    let price = f (fst most_profitable) in
    let the_good = get_o (t (fst most_profitable)) in
    let loc2 = get_o (s (snd most_profitable)) in
    let road_cost = calculate_buy_road_cost loc1 loc2 graph in
    let new_length = hypot (loc1.l_x -. loc2.l_x) (loc2.l_y -. loc2.l_y) in
    (*if road_unit_cost*.(new_length**road_length_cost_exponent) +.
      truck_price +. safe_amount <= c_info.money then *)
    if road_cost +. truck_price +. safe_amount <= c_info.money then
      ((* if edge_exists graph loc1 loc2 then print_endline "failure" else print_endline (string_of_int loc1.l_id ^ string_of_int loc2.l_id); *)
        match Map.find_all_edges graph loc1 loc2 with
        |[] ->
          Some (AddRoad {c_owner_id = c_info.p_id; l_start = loc2.l_id;
          l_end = loc1.l_id; length = new_length; c_age = 0; c_speed = 0.})
        |h :: t ->
          Some (PurchaseRoad {(s h) with c_owner_id = c_info.p_id}))
    else
      None

(*Buys a vehicle for AI*)
let buy_vehicle c_info initial_loc =
 (*  print_endline (string_of_int initial_loc.l_id); *)
  let () = Random.self_init() in
  let rand_value = Random.int (2) in
  let v_speed = if rand_value = 0 then car_speed else truck_speed in
  let v_capacity = if rand_value = 0 then car_capacity else truck_capacity in
  let v_new_t = if rand_value = 0 then Car else Truck in
  BuyVehicle {v_owner_id =c_info.p_id; speed = v_speed ;capacity =v_capacity;
    v_t = v_new_t; cargo= None; age=0; status = Waiting; x=initial_loc.l_x;
    y= initial_loc.l_y; destination = []; v_loc = Some initial_loc.l_id}

(*Returns a place to put a vehicle given the current map that will earn
 * the vehicle profits.*)
(*Uses a boolean to determine when to stop searching. *)
(* Always returns the first profitable location. (Would be tedious to do more.) *)
let get_loc_vehicle c_connections=
  let empty = Map.empty in
  let new_graph = add_edges empty c_connections in
  Map.fold_vertex (fun x y->
     if snd y then y
     else
      let goods_list = x.produces in
      let max_profits = get_good_loc new_graph goods_list 200. x in
      if f max_profits > 0. then
        (s max_profits, true)
      else
        y
  ) new_graph (None, false)

(*Determines value of all owned roads if sold*)
let sell_c_roads connections =
  List.fold_left (fun x y ->
    (road_unit_cost*.((s y).length**road_length_cost_exponent) *.sell_back_percentage)
    +. x) 0. connections

(*Determines sell value of all the vehicles owned*)
let sell_c_vehicles vehicles =
  List.fold_left (fun x y ->
    let vehicle_price =
      match y.v_t with
      |Car -> car_price
      |Truck -> truck_price in
    vehicle_price *.sell_back_percentage +. x) 0. vehicles

(*Gets a location for a vehicle *)
(*This uses the current game state to determine how the AI should make a move.
( p_id refers to the id of the AI *)
let make_c_move (state: game_state) c_id =
  let c_player = get_player c_id state.players in
  (*Current money*)
   (*  print_endline "asdf3"; *)
  let c_money = c_player.money in
  (*DEBUGGER (REMOVE LINE LATER)*)
  if c_player.p_type = Human then failwith "not a computer, TALK TO DAN" else
  let c_vehicles = get_owned_vehicles c_id state.vehicles in
  let c_connections = get_roads c_id state.graph in
  let only_c_connections = get_c_roads c_id state.graph in
  let num_only_c_connections = List.length (only_c_connections) in
  let first_v_loc =
    if num_only_c_connections > 0 then
      (f (List.hd only_c_connections)).l_id
    else (-1) in
  let c_player_info = get_p_info c_id state.players in
  let total_capacity = get_vehicle_capacity c_vehicles 0 in
  (*First, check for waiting vehicles*)
  let vehicle_processes =
    match activate_vehicles c_vehicles with
    (*Activate waiting vehicle*)
    |Some v -> make_vehicle_move v c_connections state.graph
      c_player_info.money state.ai_info c_id
    |None -> [] in
  (*Next, check if it's plausible to build a road somewhere. RETURNS AN OPTION*)
  (*ERROR*)
 (*  print_endline "ASDF"; *)
  let buy_road = buy_c_road state.graph state.ai_info c_player_info in
  (*Determine whether it's OK to sell roads + vehicles*)
  let sell_road_value = sell_c_roads only_c_connections in
  let sell_vehicle_value = sell_c_vehicles c_vehicles in
  if sell_road_value +. sell_vehicle_value +. c_money >= win_condition then
    List.map (fun x -> DeleteRoad (s x)) only_c_connections @
    List.map (fun x -> SellVehicle x) c_vehicles
  else if buy_road <> None && num_only_c_connections < max_connections then
    (if c_money > truck_price
      && total_capacity <= max_total_capacity && first_v_loc >= 0 then
      (get_o buy_road) ::
      (buy_vehicle c_player_info (get_loc_details state.graph first_v_loc))::
      vehicle_processes
    else
       (get_o buy_road) :: vehicle_processes)
  else
    ((* print_endline (string_of_bool (total_capacity <= max_total_capacity)); *)
    (if c_money > truck_price
      && total_capacity <= max_total_capacity && first_v_loc >= 0 then
    ((* print_endline "ASDF"; *)
    (buy_vehicle c_player_info (get_loc_details state.graph first_v_loc))::
    vehicle_processes)
    else if c_money > truck_price && total_capacity <= max_total_capacity then
      match fst (get_loc_vehicle c_connections) with
      |None -> vehicle_processes
      |Some loc -> (buy_vehicle c_player_info loc) :: vehicle_processes
    else
      vehicle_processes))
(*TODO: Fix buy vehicle condition (determine when not profitable)*)
  (*Requires a field that checks the average profit per vehicle and the
   *number of vehicles.*)
(*TODO: Include ref that takes into account number of roads built*)
(*TODO: move vehicles if frozen....*)
(*TODO: build more than one road*)
(*INVENTORY CHECK *)


(*Have the vehicle, how much money its made, and its steps (how much money
 *made over time) -> build more vehicles at that location *)







let init_vehicle player_id v_type start_loc_id graph=
  let loc = get_loc start_loc_id graph in
  let v =
  {
    v_owner_id = player_id;
    speed = (if v_type = Car then car_speed else truck_speed);
    capacity = (if v_type = Car then car_capacity else truck_capacity);
    v_t = v_type;
    cargo = None;
    age = 0;
    status = Waiting;
    x = loc.l_x;
    y = loc.l_y;
    destination= [];
    v_loc = Some start_loc_id;
  } in
  let () = match v.v_t with
         | Car -> print_endline "Car purchased. \n";
         | Truck -> print_endline "Truck purchased.\n"; in
  BuyVehicle(v)

let buy_road player_id l1_id l2_id graph =
  let c_length = (((get_loc l1_id graph).l_x -. (get_loc l2_id graph).l_x)**2.0 +.
    ((get_loc l1_id graph).l_y -. (get_loc l2_id graph).l_y)**2.0)**0.5 in
  try
    let connection =
      match Map.find_edge graph (get_loc l1_id graph) (get_loc l2_id graph) with
        | (_,c,_) -> c
        | _ -> failwith "invalid connection" in
    if connection.c_owner_id <> -1
    then if connection.c_owner_id = player_id
    then let () = print_endline "You already have a road here.\n" in Nothing
    else let () = print_endline "You cannot buy an opponent's road.\n" in Nothing
    else let () = print_endline "Road purchased.\n" in PurchaseRoad({connection with c_owner_id = player_id; length = c_length;})
  with
    Not_found ->
    let c =
    {
      c_owner_id = player_id;
      l_start= l1_id;
      l_end =  l2_id;
      length= c_length;
      c_age= 0;
      c_speed = 5.0; (*not used yet, completely arbitrary*)
    } in
    print_endline "Road purchased.\n"; AddRoad(c)

let sell_road player_id l1 l2 graph =
  try
    let c = Map.find_edge graph l1 l2 in
    let c' = match c with
    |(_,c,_) -> c in
    if c'.c_owner_id = player_id then DeleteRoad(c') else
    let () = print_endline "You cannot sell a road you do not own.\n" in Nothing
  with
  | _ -> Nothing

let sell_vehicle player_id v =
  if v.v_owner_id = player_id
  then (let () = match v.v_t with
         | Car -> print_endline "Car sold. \n";
         | Truck -> print_endline "Truck sold.\n"; in
         SellVehicle(v))
  else
  let () = print_endline "You cannot sell that vehicle, you do not own it!\n" in
  Nothing

let set_vehicle_dest player_id v_old start_loc end_loc st =
  let first_dest = match v_old.destination with
    | [] -> start_loc.l_id
    | h::t -> h in
  let checked_route = get_route first_dest end_loc.l_id st player_id in
  let dest_list = match checked_route with
    | None -> []
    | Some lst -> lst in
  let stat = match dest_list with
    | [] -> Waiting
    | h::t -> Driving in
  let v =
    if v_old.v_owner_id = player_id
    then {
      v_old with destination = first_dest::dest_list;
      status = stat;
    }
    else let () = print_endline "You cannot route that vehicle, you do not own it!\n" in v_old
  in
  print_endline "Vehicle enroute.\n"; SetVehicleDestination(v)

let buy_vehicle_cargo player_id v_old r_type st =
  let vehicle_max = match v_old.v_t with
    | Car -> car_capacity
    | Truck -> truck_capacity in
  match v_old.v_loc with
    | None -> print_endline "Please route your vehicle to a market before attempting t purchase goods\n"; Nothing
    | Some location ->
      let accepts = List.find (fun gp -> gp.resource = r_type) (get_loc location st.graph).produces in
      let player = List.find (fun p -> p.p_id = player_id) st.players in
      let maxq = min accepts.current vehicle_max in
      let maxq' = min maxq ( int_of_float (player.money /. accepts.price)) in
      let v =
        if v_old.v_owner_id = player_id
        then {v_old with cargo = Some {t= r_type; quantity = maxq';}}
        else let () = print_endline "You cannot purchase cargo for that vehicle, you do not own it!\n" in v_old
      in (print_endline "Cargo purchased.\n"; BuyVehicleCargo(v))
