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

(*Gets roads from player as a list*)
let get_roads p_id graph=
  Map.fold_edges_e
    (fun x y->if (Map.E.label x).c_owner_id=p_id ||
      (Map.E.label x).c_owner_id = (-1) then x::y else y) graph []

(*Allows the AI to activate waiting vehicles*)
let rec activate_vehicles v_list =
  match v_list with
  |h :: t -> if h.status = Waiting then Some h else activate_vehicles t
  |[] -> None

(*Gets details about location from graph.*)
let get_loc_details graph loc_id =
  let loc_info =
    Map.fold_vertex
    (fun x y -> if (Map.V.label x).l_id = loc_id then x::y else y) graph [] in
  match loc_info with
  |[h] -> h
  |_ -> failwith "Trying to get info about more than one loc"

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

(*Get maximum PROFIT from item in graph accessible from connections*)
(*Returns the profit, the location to sell the good to,  and the good itself *)
let get_good_profit graph good  =
  Map.fold_vertex
  (fun x y -> let profit = good_cost x.accepts good in
    if profit > f y then (profit, Some x, Some good) else y) graph
  (0., None, None)

(*See if item exists in any particular location that can be connected
 *through Dijkstra's. Find the one with the maximum price difference. *)
let rec get_good_loc graph goods_produced curr_m =
  List.fold_left
  (fun x y->
    let profits = get_good_profit graph y in
    let enough_money = curr_m >= y.price in
    if enough_money &&  f profits > f x then profits else x)
  (0., None, None) goods_produced

(*Gets stuff out of options*)
let get_o op =
  match op with
  |Some x -> x
  |None -> failwith "trying to get option out of nothing, TALK TO DAN"

(*Sees if it is possible to get from loc1 to loc2 for player*)
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


(*Stuff that chooses AI movement based on a list of connections*)
let make_vehicle_move vehicle c_connections graph curr_m=
  (*Create a new graph with only these connections.*)
  let debug = if vehicle.cargo = None then true else failwith "vehicle not empty" in
  let empty = Map.empty in
  let new_graph = add_edges empty c_connections in
  let cur_loc =
    match vehicle.v_loc with
    |None -> failwith "Passive vehicle with no location, talk to Dan"
    |Some loc -> loc in
  let loc_details = get_loc_details graph cur_loc in
  let goods = loc_details.accepts in
  let max_profits = get_good_loc graph goods curr_m in
  (*Need to fix use of 1 here: TODO*)
  if f max_profits <= 0. then
    let new_path  =
      try Dijkstra.shortest_path new_graph loc_details (get_o (s max_profits)) with
      |Not_found -> failwith "trying to get path that doesn't exist??" in
    let destinations = List.map (fun x -> (t x).l_id) (fst new_path) in
    let the_good = (get_o (t max_profits)).resource in
    [BuyVehicleCargo ({vehicle with cargo = Some ({t = the_good; quantity = 1})});
    SetVehicleDestination ({vehicle with destination = destinations})]
  else
    []

(*This uses the current game state to determine how the AI should make a move.
( p_id refers to the id of the AI *)
let make_c_move (state: game_state) c_id =
  let c_player = get_player c_id state.players in
  (*Current money*)
  let c_money = c_player.money in
  (*DEBUGGER (REMOVE LINE LATER)*)
  if c_player.p_type = Human then failwith "not a computer, TALK TO DAN" else
  let c_vehicles = get_owned_vehicles c_id state.vehicles in
  let c_connections = get_roads c_id state.graph in
  let c_player_info = get_p_info c_id state.players in
  (*First, check for waiting vehicles*)
  let vehicle_processes =
    match activate_vehicles c_vehicles with
    (*Activate waiting vehicle*)
    |Some v -> make_vehicle_move v c_connections state.graph c_player_info.money
    |None -> [] in
      vehicle_processes

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