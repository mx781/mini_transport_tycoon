type r_type =
    | Lumber
    | Iron
    | Oil
    | Electronics
    | Produce


type good = {
  t : r_type;
  quantity : int;
}

type goods_profile = {
  resource: r_type;
  steps_to_inc: int; (*how many game_steps before incrementing current by 1*)
  current: int;
  capacity: int;
  price: int;
  natural_price: int;
}

type location = {
  l_id: int;
  loc: int * int;
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

type vehicle = {
  v_owner_id: int;
  t : v_type;
  speed : float;
  capacity: int;
  cargo: good; (*For single resource type this will only have one element *)
  age: int; (*In game steps, useful for breakdowns etc.*)
  status: v_status;
  x: int;
  y: int;
(* Is a list containing the ids of the locations to be traversed in order by the
 * vehicle. The head of the list gives the current destination while the last id
 * gives the final destination. *)
  destination: int list;
  predicted_travel_time : int option (*None if the car is not moving*)
}

type connection = {
  c_owner_id: int;
  l_start: int;
  l_end: int;
  length: int;
  age: int; (*In game steps, useful for breakdowns etc.*)
  speed: int; (*speed of vehicle on road*)
}


module Location = struct
  type t = location
  let equal l1 l2 = l1.l_id = l2.l_id
  let hash = Hashtbl.hash
  let compare e1 e2 =
    if e1.l_id > e2.l_id then 1
    else if e1.l_id = e2.l_id then 0
    else (~-1)
end

module Connection = struct
  type t = connection
  let compare e1 e2 =
    if e1.length > e2.length then 1
    else if e1.length = e2.length then 0
    else (~-1)
  let equal e1 e2 = e1.l_start = e2.l_start && e1.l_end = e2.l_end
  let hash = Hashtbl.hash
  let default = {
    c_owner_id = 4;
    l_start= 0;
    l_end =  1;
    length= 2;
    age= 0;
    speed= 3;
  }
end

module Map = Graph.Persistent.Graph.ConcreteLabeled(Location)(Connection)


type game_state = {
  vehicles : vehicle list;
  graph: Map.t;
  players : Player.player list;
  game_age : int;
  paused: bool;
}

let form_connection map player_id loc1 loc2 =
  let new_connect = {c_owner_id = player_id; l_start = 0; l_end = 1;
    age = 0; speed = 5; length = 5} in (*Placeholder*)
  let start_loc = loc1 in
  let end_loc = loc2 in
  Map.add_edge_e map (loc1, new_connect, loc2)

let route_vehicle l v v_list =
  let new_v = {v with destination = [l.l_id]} in
  List.map (fun x -> if x.v_owner_id = v.v_owner_id then new_v else x) v_list

let update_vehicles v_lst =
  v_lst

let update_connections c_lst =
  c_lst

let update_locations l_lst =
  l_lst
