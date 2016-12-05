open GameElements
open Player

(*A type representing all the different choices the user can make*)
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

(*AI constants*)
let min_bought = 10
let large_float = 200000.0
let small_float = 0.0
let max_total_capacity = 201
let safe_amount = 20.0
let min_profit = 4.0 (*minimum profit for AI to build a road*)
let max_connections = 3 (*maximum number of connections for AI*)

(*Using a player ID and a list of players, returns the information behind
 *the player (whether it's an AI, and the money it has) *)
(*Precondition: one of the players in the list of players has the player ID*)
let rec get_player p_id (p_list:Player.player list)=
  match p_list with
  |h :: t-> if h.p_id = p_id then h else get_player p_id t
  |[]-> failwith "trying to get player when non-existent, TALK TO DAN"

(*Given a player id and a vehicle list, returns the vehicles that the player
 *owns *)
let rec get_owned_vehicles p_id v_list =
  List.fold_left(fun x y -> if y.v_owner_id = p_id then y :: x else x) [] v_list

(*Helper functions used to get things out of triples.*)
let f (x,_,_) = x
let s (_,y,_) = y
let t (_,_,z) = z

(*Given a player ID and a graph, this function returns the connections that
 *are either owned by the player or public.*)
let get_roads p_id graph=
  Map.fold_edges_e
    (fun x y->if (Map.E.label x).c_owner_id=p_id ||
      (Map.E.label x).c_owner_id = (-1) then x::y else y) graph []

(*The same as get_roads, but only returns the roads that the player owns. *)
let get_c_roads p_id graph =
Map.fold_edges_e
    (fun x y->if (Map.E.label x).c_owner_id=p_id then x::y else y) graph []

(*This function takes in a vehicle list and returns None if a no vehicles are
 *waiting or Some vehicle if a vehicle is waiting. It only returns one vehicle
 *at a time, however.*)
let rec activate_vehicles v_list =
  match v_list with
  |h :: t ->  if h.status = Waiting && h. cargo = None then Some h
              else activate_vehicles t
  |[] -> None

(*Gets info about a player from an int id*)
let rec get_p_info id players =
  match players with
  |h :: t -> if h.p_id = id then h else get_p_info id t
  |[] -> failwith "cannot find player ID"

(*Adds edges to graph*)
let rec add_edges graph edge_list =
  match edge_list with
  |[] -> graph
  |h :: t -> add_edges (Map.add_edge_e graph h) t

(*Get profit from good in particular location*)
let rec good_cost (goods_list:goods_profile list) (good:goods_profile)=
  match goods_list with
  |h :: t ->
    if h.resource = good.resource then (h.price -. good.price)
    else good_cost t good
  |[] -> 0.

(*Get maximum PROFIT from an item in graph accessible from connections *)
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
    then (profit, Some x, Some good) else y)
  graph (0., None, None)

(*See if item exists in any particular location that can be connected
 *through Dijkstra's. Find the one with the maximum price difference.
 *Produces a triple with the profit, the location to sell the good to, and the
 * good itself *)
let rec get_good_loc graph goods_produced curr_m curr_loc =
  List.fold_left
  (fun x y->
    let profits = get_good_profit graph y curr_loc in
    let enough_money = curr_m >= y.price in
    if enough_money &&  f profits > f x then profits else x)
  (0., None, None) goods_produced

(*Gets an object out of an option.
 *Precondition: the value of the option is not None.*)
let get_o op =
  match op with
  |Some x -> x
  |None -> failwith "trying to get option out of nothing"

(*Sees if it is possible to get from loc1 to loc2 for player with p_id*)
let get_route (loc1:int) (loc2:int) state (p_id:int)=
  let accessible_roads = get_roads p_id state.graph in
  let empty = Map.empty in
  let new_graph = add_edges empty accessible_roads in
  let loc_info1 = get_loc loc1 state.graph in
  let loc_info2 = get_loc loc2 state.graph in
  let new_path =
    (try Dijkstra.shortest_path new_graph loc_info1 loc_info2 with
    |_ ->([], (-1.))) in
  if new_path = ([],(-1.)) then
    None
  else
    Some (List.map (fun x -> (t x).l_id) (fst new_path))


(*Gets the maximum quantity that can taken by a vehicle. Returns an int
 * corresponding to the quantity accepted by a vehicle.*)
let get_quantity vehicle r_type location curr_money=
  let vehicle_max = match vehicle.v_t with
    | Car -> car_capacity
    | Truck -> truck_capacity in
  let accepts = List.find (fun gp -> gp.resource = r_type) location.produces in
  let maxq = min accepts.current vehicle_max in
  let maxq'= min maxq (int_of_float (curr_money /. accepts.price)) in
    maxq'

(*Gets a new location close to the vehicle. First checks if other locations are
 * accessible.*)
let get_new_dest graph_access curr_loc =
  Map.fold_edges_e (fun x y -> let () = Random.self_init() in
    let r = Random.int (2) in
    if r = 0 || y = None then
      (if (f x).l_id = curr_loc.l_id then Some (t x)
      else if (t x).l_id = curr_loc.l_id then Some (f x)
      else y)
    else y) graph_access None


(*Identifies the behavior of a waiting vehicle. It returns a process
 * corresponding to how the vehicle should act. *)
let make_vehicle_move vehicle c_connections graph curr_m c_id =
  (*Create a new graph with only these connections.*)
  let empty = Map.empty in
  let new_graph = add_edges empty c_connections in
  let cur_loc =
    match vehicle.v_loc with
    |None -> failwith "Passive vehicle with no location"
    |Some loc -> loc in
  let loc_details = get_loc cur_loc graph in
  let goods = loc_details.produces in
  let max_profits = get_good_loc new_graph goods curr_m loc_details in
  if f max_profits > 0. then
    let the_good = (get_o (t max_profits)).resource in
    let new_loc = get_o (s max_profits) in
    let q = get_quantity vehicle the_good loc_details curr_m in
    if q > 0 then
      let new_path =
        try Dijkstra.shortest_path new_graph loc_details new_loc with
        |Not_found -> failwith "trying to get path that doesn't exist?" in
      let destinations = List.map (fun x -> (t x).l_id) (fst new_path) in
      let new_cargo_vehicle =
        {vehicle with cargo = Some ({t = the_good; quantity = q})} in
      let new_dest_vehicle =
        {new_cargo_vehicle with destination = destinations; status = Driving} in
      [BuyVehicleCargo new_cargo_vehicle;SetVehicleDestination new_dest_vehicle]
    else []
  else
    match get_new_dest new_graph loc_details with
    |None -> [SellVehicle vehicle]
    |Some loc1 ->
      [SetVehicleDestination
      ({vehicle with destination = [loc1.l_id]; status = Driving})]

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
  Map.fold_edges_e
  (fun x y ->
    if y = true || ((f x).l_id = initial_loc.l_id &&
      (t x).l_id = final_loc.l_id || (f x).l_id = final_loc.l_id &&
      (t x).l_id = initial_loc.l_id)  && (s x).c_owner_id <> (-1)  then true
    else false)
  graph false

(*Used to determine whether a road can be sold or bought; it exists for the use
 * of good_loc_info. Var is_greater_than is used to determine the comparison
 * function to be used. *)
let better_deal is_greater_than info_compare info_other new_price new_good
  new_loc money graph=
  let comp = if is_greater_than then (>) else (<) in
  if s info_other <> None then
    let y_loc = get_o (s (info_other)) in
    let road_cost = calculate_buy_road_cost new_loc y_loc graph in
    (if comp new_price (f info_compare) && not(edge_exists graph new_loc y_loc)
      && road_cost +. safe_amount +. truck_price <= money then
      (new_price, Some new_loc, Some new_good)
    else
      info_compare)
  else
    (if comp new_price (f info_compare) then
      (new_price, Some new_loc, Some new_good)
    else info_compare)

(*Returns the price, location option, and good option describing the location
 *with a good at the cheapest and most expensive prices (produces/accepts).
 *Good to sell first; good to sell second*)
let good_loc_info graph good money=
  Map.fold_vertex (fun x y ->
    let cheaper =
    match get_good_cost x.produces good with
    |None -> fst y
    |Some price ->
       better_deal false (fst y) (snd y) price good x money graph in
    let expensive =
    match get_good_cost x.accepts good with
    |None -> snd y
    |Some price ->
      better_deal true (snd y) (fst y) price good x money graph in
      (cheaper,expensive))
  graph ((large_float, None, None),(small_float, None, None))

(*Returns the price, location option, and good option containing the cheapest
 *and most expensive goods.
 *Used for profit calculations.*)
let get_good_diffs graph goods c_info=
  List.map (fun x -> good_loc_info graph x c_info.money) goods

(*Since get_good_locs returns the greatest profit possible for a list of
 *goods, get_greatest_dif returns the greatest profit available for any
 *set of goods.*)
let get_greatest_dif good_dif graph =
  List.fold_left (fun x y -> let new_diff = (f (snd y) -. f (fst y)) in
    let old_diff = (f (snd x) -. f (fst x)) in
    let has_been_built =
      if s (snd y) <> None && s (fst y) <> None then
        edge_exists graph (get_o (s (snd y))) (get_o (s (fst y)))
      else true in
    if old_diff < new_diff && (not has_been_built) then y else x)
    ((large_float, None, None),(small_float, None, None)) good_dif

(* Using the current graph and player info, determines whether or not to buy
 * a road.
 *  *)
let buy_c_road graph c_info=
  let goods = [Lumber; Iron; Oil; Electronics; Produce] in
  (*Gets cheapest out of all locations*)
  let price_difs = get_good_diffs graph goods c_info in
  let most_profitable = get_greatest_dif price_difs graph in
  if s (fst most_profitable) = None || s (snd most_profitable) = None
    || f (snd most_profitable) -. f (fst most_profitable) <= min_profit then
    None
  else
    let loc1 = get_o (s (fst most_profitable)) in
    let loc2 = get_o (s (snd most_profitable)) in
    let road_cost = calculate_buy_road_cost loc1 loc2 graph in
    let new_length = hypot (loc1.l_x -. loc2.l_x) (loc2.l_y -. loc2.l_y) in
    if road_cost +. truck_price +. safe_amount <= c_info.money then
      (match Map.find_all_edges graph loc1 loc2 with
      |[] ->
        Some (AddRoad {c_owner_id = c_info.p_id; l_start = loc2.l_id;
        l_end = loc1.l_id; length = new_length; c_age = 0; c_speed = 0.})
      |h :: t ->
        Some (PurchaseRoad {(s h) with c_owner_id = c_info.p_id}))
    else
      None

(*Buys a vehicle for AI. Whether it's a car or a truck is chosen randomly.*)
let buy_vehicle c_info initial_loc =
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
(* Always returns the first profitable location.*)
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
      else y
  ) new_graph (None, false)

(*Determines value of all owned roads if sold*)
let sell_c_roads connections =
  List.fold_left (fun x y ->
    (road_unit_cost*.((s y).length**road_length_cost_exponent)
    *.sell_back_percentage)+. x)
  0. connections

(*Determines sell value of all the vehicles owned*)
let sell_c_vehicles vehicles =
  List.fold_left (fun x y ->
    let vehicle_price =
      match y.v_t with
      |Car -> car_price
      |Truck -> truck_price in
    vehicle_price *.sell_back_percentage +. x) 0. vehicles

(*This uses the current game state to determine how the AI should make a move.
( p_id refers to the id of the AI. *)
let make_c_move (state: game_state) c_id =
  let c_player = get_player c_id state.players in
  let delay = match c_player.p_type with
    | Human -> failwith "human not expected"
    | AI(l) -> 400/l in
  if (Random.int delay <> 0) then [] else
  let c_money = c_player.money in
  if c_player.p_type = Human then failwith "not a computer" else
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
  (*First, check for waiting vehicles and activate them.*)
  let vehicle_processes =
    match activate_vehicles c_vehicles with
    |Some v -> make_vehicle_move v c_connections state.graph
      c_player_info.money c_id
    |None -> [] in
  (*Determine whether to buy roads or not*)
  let buy_road = buy_c_road state.graph c_player_info in
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
      (buy_vehicle c_player_info (get_loc first_v_loc state.graph ))::
      vehicle_processes
    else
       (get_o buy_road) :: vehicle_processes)
  else
    (if c_money > truck_price
      && total_capacity <= max_total_capacity && first_v_loc >= 0 then
    (buy_vehicle c_player_info (get_loc first_v_loc state.graph))::
    vehicle_processes
    else if c_money > truck_price && total_capacity <= max_total_capacity then
      match fst (get_loc_vehicle c_connections) with
      |None -> vehicle_processes
      |Some loc -> (buy_vehicle c_player_info loc) :: vehicle_processes
    else
      vehicle_processes)

(* init vehicle creates a vehicle creation process based on the player_id,
 * the type of vehicle v_type, the startig position location's id start_loc_id,
 * and a graph of locations and edges graph *)
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
  BuyVehicle(v)

(* [buy road player_id l1 l2 graph] creates an AddRoad or PurchaseRoad
 * process from a player_id of the purchaser, the location ids of the start
 * and end points, and the graph of connections and locations. It returns a
 * Nothing process if the road cannot be purchased.
 *)
let buy_road player_id l1 l2 graph =
  let c_length = ((l1.l_x -. l2.l_x)**2.0
    +. (l1.l_y -. l2.l_y)**2.0)**0.5 in
  try
    let connection =
      match Map.find_edge graph l1 l2 with
        | (_,c,_) -> c in
    if connection.c_owner_id <> -1
    then if connection.c_owner_id = player_id
    then (print_endline "You already have a road here.\n"; Nothing)
    else (print_endline "You cannot buy an opponent's road.\n"; Nothing)
    else (print_endline "Road purchased.\n";
      PurchaseRoad(
        {connection with c_owner_id = player_id; length = c_length;}
      ))
  with
    Not_found ->
    let c =
    {
      c_owner_id = player_id;
      l_start= l1.l_id;
      l_end =  l2.l_id;
      length= c_length;
      c_age= 0;
      c_speed = 5.0; (*not used yet, completely arbitrary*)
    } in
    print_endline "Road purchased.\n"; AddRoad(c)

(* [sell_road player_id l1 l2 graph] creates a SellRoad
 * process from a player_id of the seller, the location ids of the start
 * and end points, and the graph of connections and locations. It returns a
 * Nothing process if the road cannot be sold.
 *)
let sell_road player_id l1 l2 graph =
  try
    let c = Map.find_edge graph l1 l2 in
    let c' = match c with
    |(_,c,_) -> c in
    if c'.c_owner_id = player_id
    then ( print_endline "Road sold.\n"; DeleteRoad(c'))
    else
    let () = print_endline "You cannot sell a road you do not own.\n" in Nothing
  with
  | _ -> Nothing

(* [sell_vehicle player_id v] creates a SellVehicle
 * process from a player_id of the seller, and the vehicle v to sell. It returns
 * a Nothing process if the road cannot be sold.
 *)
let sell_vehicle player_id v =
  if v.v_owner_id = player_id
  then (let () = match v.v_t with
         | Car -> print_endline "Car sold. \n";
         | Truck -> print_endline "Truck sold.\n"; in
         SellVehicle(v))
  else
  let () = print_endline "You cannot sell that vehicle, you do not own it!\n" in
  Nothing

(* [set_vehicle_dest player_id v_old start_loc end_loc st] creates a
 * SetVehicleDestination process from a player_id of the seller, the old vehicle
 * that we are setting the destination of v_old, the starting location start_loc
 * and the ending location end_loc. It returns a Nothing process if the vehicle
 * cannot be routed.
 *)
let set_vehicle_dest player_id v_old start_loc end_loc st =
  let first_dest = match v_old.destination with
    | [] -> start_loc.l_id
    | h::t -> h in
  let checked_route = get_route first_dest end_loc.l_id st player_id in
  let dest_list = match checked_route with
    | None -> (print_endline "Vehicle cannot reach this location!\n";
      v_old.destination)
    | Some lst -> (print_endline "Vehicle en route.\n"; lst) in
  let stat = match dest_list with
    | [] -> Waiting
    | h::t -> Driving in
  let v =
    if v_old.v_owner_id = player_id
    then
    {
      v_old with destination = first_dest::dest_list;
      status = stat;
    }
    else let () =
      print_endline "You cannot route that vehicle, you do not own it!\n" in
      v_old
  in
  SetVehicleDestination(v)

(* [buy_vehicle_cargo player_id v_old r_type st] creates a BuyVehicle process
 * based on the player_id of the purchaser, the old vehicle who will have the
 * cargo added to it v_old, the resource type to buy r_type, and the game state
 * st. It returns a Nothing process if the cargo cannot be purchased.*)
let buy_vehicle_cargo player_id v_old r_type st =
  let vehicle_max = match v_old.v_t with
    | Car -> car_capacity
    | Truck -> truck_capacity in
  match v_old.v_loc with
    | None -> print_endline ("Please route your vehicle to a market before" ^
      " attempting to purchase goods\n"); Nothing
    | Some location ->
      let accepts = List.find (fun gp -> gp.resource = r_type)
        (get_loc location st.graph).produces in
      let player = List.find (fun p -> p.p_id = player_id) st.players in
      let maxq = min accepts.current vehicle_max in
      let maxq' = min maxq ( int_of_float (player.money /. accepts.price)) in
      let v =
        if v_old.v_owner_id = player_id
        then {v_old with cargo = Some {t= r_type; quantity = maxq';}}
        else let () = print_endline ("You cannot purchase cargo for" ^
          " that vehicle, you do not own it!\n") in v_old
      in BuyVehicleCargo(v)
