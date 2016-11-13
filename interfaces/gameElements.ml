open Graph

type r_type =
    | Lumber
    | Iron
    | Oil
    | Electronics
    | Produce

type v_type =
    |Car
    |Truck

type v_status =
  | Waiting
  | Driving
  | Broken (*Equivalent to waiting but inaccessible*)

type good = {
  t : r_type;
  quantity : int;
}

type goods_profile = {
  resource: r_type;
  steps_to_inc: int;
  current: int;
  capacity: int;
  price: int;
  natural_price: int;
}

type location = {
  id: int;
  location: int * int;
  accepts: goods_profile list;
  produces: goods_profile list;
}

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
  l_start: location;
  l_end: location;
  age: int; (*In game steps, useful for breakdowns etc.*)
  speed: int -> int; (*speed of vehicle on road*)
  length: int;
}

module Location = struct
  type t = location
  let equal l1 l2 = l1.id = l2.id
  let hash = Hashtbl.hash
  let compare e1 e2 =
    if e1.id > e2.id then 1
    else if e1.id = e2.id then 0
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
  let default = failwith "something about default connections; talk to Dan"
end

module Map = Persistent.Graph.ConcreteLabeled(Location)(Connection)

let form_connection map player_id loc1 loc2 =
  let new_connect = {c_owner_id = player_id; l_start = loc1; l_end = loc2;
    age = 0; speed = (fun x -> 5); length = 5} in (*Placeholder*)
  let start_loc = loc1 in
  let end_loc = loc2 in
  Map.add_edge_e map (loc1, new_connect, loc2)

let update_connections c c_list = failwith "unimplemented"

let update_vehicles l v v_list =
  let new_v = {v with destination = [l.id]} in
  List.map (fun x -> if x.v_owner_id = v.v_owner_id then new_v else x) v_list

let check_vehicle_status vehicle = failwith "unimplemented"

let update_locations l l_list = failwith "unimplemented"

let location_info l = failwith "unimplemented"
