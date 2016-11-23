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

type vehicle = {
  v_owner_id: int;
  t : v_type;
  speed : float;
  capacity: int;
  cargo: good; (*For single resource type this will only have one element *)
  age: int; (*In game steps, useful for breakdowns etc.*)
  status: v_status;
  x: float;
  y: float;
(* Is a list containing the ids of the locations to be traversed in order by the
 * vehicle. The head of the list gives the current destination while the last id
 * gives the final destination. *)
  destination: int list;
}

type connection = {
  c_owner_id: int;
  l_start: int;
  l_end: int;
  length: float;
  c_age: int; (*In game steps, useful for breakdowns etc.*)
  c_speed: float; (*speed of vehicle on road*)
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
    length= 2.0;
    c_age= 0;
    c_speed= 3.0;
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

let breakdown_chance = 0.0001

let form_connection map player_id loc1 loc2 =
  let new_connect = {c_owner_id = player_id; l_start = 0; l_end = 1;
    c_age = 0; c_speed = 5.0; length = 5.0} in (*Placeholder*)
  let start_loc = loc1 in
  let end_loc = loc2 in
  Map.add_edge_e map (loc1, new_connect, loc2)

let route_vehicle l v v_list =
  let new_v = {v with destination = [l.l_id]} in
  List.map (fun x -> if x.v_owner_id = v.v_owner_id then new_v else x) v_list

let update_driving_v graph v =
  let dest = Map.fold_vertex
    (fun x lst -> if x.l_id = (List.hd v.destination) then x :: lst else lst) graph [] |> List.hd in
  let dest_x = dest.l_x in
  print_endline (string_of_float dest_x);
  let dest_y = dest.l_y in
  print_endline (string_of_float dest_y);
  let delta_x = (dest_x -. v.x) in
  let delta_y = (dest_y -. v.y) in
  let new_x = v.x +. (v.speed *. (cos (atan2 delta_y delta_x))) in
  let new_y = v.y +. (v.speed *. (sin (atan2 delta_y delta_x))) in
  let new_status =
    if (((v.x -. dest_x)*.(v.x -. dest_x)) +. ((v.y -. dest_y)*.(v.y -. dest_y))) < v.speed *. v.speed
    then Waiting
    else if Random.float 1.0 < breakdown_chance then Broken else Driving in
  {
  v_owner_id = v.v_owner_id;
  t = v.t;
  speed = v.speed;
  capacity = v.capacity;
  cargo = v.cargo; (*For single resource type this will only have one element *)
  age= v.age + 1; (*In game steps, useful for breakdowns etc.*)
  status= new_status;
  x = new_x;
  y = new_y;
  destination = v.destination;
  }

let update_waiting v =
  {v with age = v.age + 1}

(*Currently only works for driving vehicles*)
let update_vehicle graph v =
  match v.status with
    | Driving -> update_driving_v graph v
    | Waiting -> update_waiting v
    | Broken -> update_waiting v


let update_vehicles v_lst graph =
 List.map (update_vehicle graph) v_lst

let update_connections c_lst =
  c_lst

let update_locations l_lst =
  l_lst
