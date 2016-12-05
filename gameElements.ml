open Player

(* Type definitions *)
type r_type =
    | Lumber
    | Iron (*really drugs, but these constructors are all dummy names so it does
            * not need changed. *)
    | Oil
    | Electronics
    | Produce

type loc_id = int

type player_id = int

type good = {
  t : r_type;
  quantity : int;
}

type goods_profile = {
  resource: r_type;
  steps_to_inc: int; (*how many game_steps before incrementing current by 1*)
  current: int;
  capacity: int;
  price: float;
  natural_price: float;
}

type location = {
  l_id: int;
  l_x: float;
  l_y: float;
  accepts: goods_profile list;
  produces: goods_profile list;
}

type v_type =
  | Car
  | Truck

type v_status =
  | Waiting
  | Driving
  | Broken (*Equivalent to waiting but inaccessible*)
  (* | ToDrop Selling something *)

type vehicle = {
  v_owner_id: int;
  speed : float;
  capacity : int;
  v_t : v_type;
  cargo: good option;
  age: int; (*In game steps, useful for breakdowns etc.*)
  status: v_status;
  x: float;
  y: float;
(* Is a list containing the ids of the locations to be traversed in order by the
 * vehicle. The head of the list gives the current destination while the last id
 * gives the final destination. *)
  destination: int list;
  v_loc: int option;
}

type connection = {
  (*IF THE OWNER IS -1 then that means there is no owner*)
  c_owner_id: int;
  l_start: int;
  l_end: int;
  length: float;
  c_age: int; (*In game steps, useful for breakdowns etc.*)
  c_speed: float; (*speed of vehicle on road*)
}

(*Location module which will be used to create a Graph module for using
 * Ocamlgraph (so that we have Dijkstra's algorithm and easy map management.)*)
module Location = struct
  type t = location
  type label = location
  let equal l1 l2 = l1.l_id = l2.l_id
  let hash = Hashtbl.hash
  let compare e1 e2 =
    if e1.l_id > e2.l_id then 1
    else if e1.l_id = e2.l_id then 0
    else (~-1)
end

(*Connection module which will be used to create a Graph module for using
 * Ocamlgraph (so that we have Dijkstra's algorithm and easy map management.)*)
module Connection = struct
  type t = connection
  let compare e1 e2 =
    if e1.length > e2.length then 1
    else if e1.length = e2.length then 0
    else (~-1)
  let equal e1 e2 = e1.l_start = e2.l_start && e1.l_end = e2.l_end
  let hash = Hashtbl.hash
  let default = {
    c_owner_id = (-1);
    l_start= 0;
    l_end =  0;
    length= 2.0;
    c_age= 0;
    c_speed= 3.0;
  }
end

(* module using ocamlgraph so that we have a graph type and
 * built in graph functions*)
module Map = Graph.Persistent.Graph.ConcreteLabeled(Location)(Connection)


type game_state = {
  vehicles : vehicle list;
  graph: Map.t;
  game_age : int;
  paused: bool;
  mutable players : Player.player list;
}

(*Connection weights module is used for Dijkstra's algorithm since connections
 * need weights associated with them.*)
module ConnectionWeight = struct
  type edge = Map.E.t
  type t = float
  let weight (_,e,_) : float = e.length
  let compare = Pervasives.compare
  let add e1 e2 = e1 +. e2
  let zero = 0.
end

(*module for Dijkstra's algorithm with our custom ConnectionWeight module*)
module Dijkstra = Graph.Path.Dijkstra(Map)(ConnectionWeight)

(* The below constants are all used for a variety of rules that change the
 * gameplay. They are almost all exactly what their name states, and are
 * clarified when there is ambiguity*)
let fps = 24.0
let car_price = 100.0
let truck_price = 200.0
let sell_back_percentage = 0.6
let road_unit_cost = 0.3
(* Used for determining how much faster
   the road cost increases with distance of the road.*)
let road_length_cost_exponent = 1.2
let road_rights_unit_cost = 0.65
let win_condition = 2500.0

(* Breakdowns were determined to not be fun or adding of any strategic value,
 * so while cars will breakdown if this constant is raised we leave it at 0.0
 * to keep vehicles from ever breaking down.*)
let breakdown_chance = 0.0
let car_speed = 4.0
let truck_speed = 2.0
let car_capacity = 25
let truck_capacity = 100
let price_update_steps = 10
let buy_vehicle_condition = 2.

let ai_max_level = 500
let easy_ai_level = 1
let medium_ai_level = 4
let hard_ai_level = 16
let brutal_ai_level = 400

(* Using a location ID and a graph, get_loc returns the location details.
 * Precondition: A location in the graph has the specific location ID.
 * *)
let get_loc id graph =
  let vertex_lst = Map.fold_vertex
    (fun v lst -> if v.l_id = id then v :: lst else lst) graph [] in
  match vertex_lst with
  |h :: t -> h
  |[] -> failwith "trying to get a vertex that doesn't exist"


(* returns new player list updated for sold cargo. Cargo is emptied in calling
 * function only, this just does the money transfer*)
let sell_cargo v g players graph st =
  let player = List.find (fun p -> p.p_id = v.v_owner_id) players in
  let sell_l = Map.fold_vertex
    (fun l opt -> let loc = Map.V.create l in
                  if loc.l_x = v.x && loc.l_y = v.y
                  then Some loc
                  else opt)
    graph None in
  let sell_l' = match sell_l with
    | None -> failwith "location does not exist"
    | Some l -> l in
  try
    let sell_price =
    (List.find (fun gp -> gp.resource = g.t) sell_l'.accepts).price in
    let new_money = (float_of_int g.quantity) *. sell_price in
    let player' = {player with money = player.money +. new_money} in
    st.players <- List.map (fun p -> if p = player then player' else p) players;
    true
  with _ -> false

(*[distance l1 l2] is the distance between two locations l1 and l2*)
let distance l1 l2 =
  ((l1.l_x -. l2.l_x)**2.0 +. (l1.l_y -. l2.l_y)**2.0)**0.5

(* [calculate_sell_road_cost start_l end_l] returns the money that a player will
 * earn from selling a given road with start location start_l and end location
 * end_l*)
let calculate_sell_road_cost start_l end_l =
  let length = distance start_l end_l in
  (road_unit_cost*.(length**road_length_cost_exponent)) *.sell_back_percentage

(* [calculate_buy_road_cost start_l end_l] returns the money that a player will
 * pay to buy a road with start location start_l and end location end_l in
 * graph graph, whether this would involve building it from scratch or buying it
 * if it is public.*)
let calculate_buy_road_cost start_l end_l graph =
  let length = distance start_l end_l in
  try ignore (Map.find_edge graph start_l end_l);
    (road_rights_unit_cost *. length)
  with Not_found ->
    (road_unit_cost*.(length**road_length_cost_exponent))

(* pre: players is a list of players in the game and contains a player with id
 *        corresponding to the v_owner_id of v,
 *      graph is a valid graph with at least two locations and one edge,
 *      v is a vehicle record with status = Driving
*        and is contained in the vehicle list of st,
 *      st is a valid game state, that is, there are no duplicate
 *        location or player ids in the player list or graph.
 * post: [update_driving_v players graph v st] updates a single driving vehicle
 *       for one frame of the game, based on its current location, cargo, etc.
 *)
let update_driving_v players graph v st =
  let dest = Map.fold_vertex
    (fun x lst -> if x.l_id = (List.hd v.destination) then x :: lst else lst)
    graph [] |> List.hd in
  let dest_x = dest.l_x in
  let dest_y = dest.l_y in
  let delta_x = (dest_x -. v.x) in
  let delta_y = (dest_y -. v.y) in
  let new_x = v.x +. (v.speed *. (cos (atan2 delta_y delta_x))) in
  let new_y = v.y +. (v.speed *. (sin (atan2 delta_y delta_x))) in
  if (((v.x -. dest_x)*.(v.x -. dest_x)) +.
     ((v.y -. dest_y)*.(v.y -. dest_y))) < v.speed *. v.speed
  then
    (match v.destination with
      | [] -> failwith "destinations should not be "
      | h::[] ->
                 let v' =
                 { v with x = dest_x; y = dest_y;
                   cargo = None;
                   age = v.age + 1;
                   destination = [];
                   status = Waiting;
                   v_loc = Some h
                 } in
                 let v'' = match v.cargo with
                    | None -> v'
                    | Some g ->
                      {v' with cargo = if sell_cargo v' g players graph st
                                       then None
                                       else Some g} in
                 v''
      | h::h2::t ->
        try
          let _ =
            (Map.find_edge graph (get_loc h graph) (get_loc h2 graph)) in
          { v with x = dest_x; y = dest_y;
            age = v.age + 1;
            destination = h2::t;
            v_loc = Some h
          }
        with Not_found ->
          {v with age = v.age + 1; destination = []; status = Waiting}
        | _ -> failwith "unexpected vehicle destination pattern")
  else if Random.float 1.0 < breakdown_chance then
    {v with age = v.age + 1; status = Broken}
  else
    {v with age = v.age + 1; x = new_x; y = new_y}

(* [update_waiting v] takes in a vehicle that has status waiting (or broken)
 * and returns an updated vehicle updated for one frame of gameplay.*)
let update_waiting v =
  {v with age = v.age + 1}

(* pre: players is a list of players in the game and contains a player with id
 *        corresponding to the v_owner_id of v,
 *      graph is a valid graph with at least two locations and one edge,
 *      v is a vehicle record and is contained in the vehicle list of st
 *      st is a valid game state, that is, there are no duplicate
 *        location or player ids in the player list or graph.
 * post: [update_vehicle graph players st v] updates a single vehicle
 *       for one frame of the game, based on its current status, location,
 *       cargo, etc.
 *)
let update_vehicle graph players st v =
  match v.status with
    | Driving -> update_driving_v players graph v st
    | Waiting -> update_waiting v
    | Broken -> update_waiting v

(* pre: players is a list of players in the game and contains a player with id
 *        corresponding to the v_owner_id of v,
 *      graph is a valid graph with at least two locations and one edge,
 *      v_lst is a list of vehicle records, exactly the vehicle list in st,
 *      st is a valid game state, that is, there are no duplicate
 *        location or player ids in the player list or graph.
 * post: [update_vehicles v_lst graph players st] updates all vehicles in v_lst
 *       for one frame of the game, based on its current status, location,
 *       cargo, etc.
 *)
let update_vehicles v_lst graph players st =
 List.map (update_vehicle graph players st) v_lst

(* [new_gp g_a gp] updates a goods profile gp based on the game age g_a and
 * using random fluctutations in the prices of goods
 *)
let new_gp g_a gp =
  { gp with
  current = min
    (gp.current + (if g_a mod gp.steps_to_inc = 0 then 1 else 0)) (gp.capacity);
  price = max (gp.price +. if g_a mod price_update_steps = 0
    then (0.04*. (2.0*.(Random.float gp.natural_price) -. gp.natural_price))
    else 0.0) 0.0;
  }

(* [update_location age l] takes in an individual age (game_age) and location l
 * and updates the location accordingly (i.e. their good profiles)*)
let update_location age l =
  { l with
  accepts= List.map (new_gp age) l.accepts;
  produces= List.map (new_gp age) l.produces;
  }

(* [add_vertices m locs] adds locs to map (graph) m*)
let rec add_vertices m locs =
  match locs with
    | [] -> m
    | h::t -> (add_vertices (Map.add_vertex m h) t)

(* [update_location graph g_age] returns a new graph after updating each
 * locations in the graph for one frame in the game.*)
let update_locations graph g_age =
  Map.map_vertex (update_location g_age) graph

(*Gets the price of a good according to location*)
let rec find_good_price g_p (resource:good) =
  match g_p with
  |h :: t ->
    if h.resource = resource.t then h.price else find_good_price t resource
  |[] -> failwith "trying to sell good at wrong loc, TALK TO DAN"

(*Buys the vehicle associated with a player and adds it to v_list*)
let buy_vehicle vehicle player location v_list spd cpt =
  let new_vehicle = {v_owner_id = player; v_t = vehicle; cargo = None;
    status = Waiting; x = location.l_x; y = location.l_y; destination = [];
    speed = spd; age = 0; capacity = cpt; v_loc = None} in
  new_vehicle :: v_list

(* [set_p_dif gd p] sets the difficulty level of a player p (if it is AI) given
 * a game difficulty gd.*)
let set_p_dif gd p =
  match (p.p_type,gd) with
    | (Human,_) -> p
    | (AI(_),Easy) -> {p with p_type = AI(easy_ai_level)}
    | (AI(_),Medium) -> {p with p_type = AI(medium_ai_level)}
    | (AI(_),Hard) -> {p with p_type = AI(hard_ai_level)}
    | (AI(_),Brutal) -> {p with p_type = AI(brutal_ai_level)}

(* [set_game_difficulty gd st] sets the game difficulty based on a game
 * difficulty gd and a game state st*)
let set_game_difficulty gd st =
  let new_players = List.map (set_p_dif gd) st.players in
  {st with players = new_players}
