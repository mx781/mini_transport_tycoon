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
  l_start: location;
  l_end: location;
  length: int;
  age: int; (*In game steps, useful for breakdowns etc.*)
  speed: int; (*speed of vehicle on road*)
}


module Location = struct
  type t = location

  let equal (l1:location) (l2:location) =  true
  let hash l = 1
  let compare l1 l2 = 1

end

module Connection = struct
  type t = connection

  let compare c1 c2 =
    if c1.length > c2.length then 1 else if c1.length = c2.length then 0 else -1
  let default =  {c_owner_id=0;l_start = {loc=(1,1);accepts= [];produces = []};l_end={loc=(1,1);accepts= [];produces = []} ;length =0;age =0;speed=0}

end

module Map = Graph.Persistent.Graph.ConcreteLabeled(Location)(Connection)
(* module RandGraph = Graph.Rand.Planar.P(Map) *)


type game_state = {
  vehicles : vehicle list;
  graph: Map.t;
  players : Player.player list;
  game_age : int;
  paused: bool;
}

let v_update v_lst =
  v_lst

let c_update c_lst =
  c_lst

let l_update l_lst =
  l_lst
