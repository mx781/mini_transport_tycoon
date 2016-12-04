open Player


type r_type =
    | Lumber
    | Iron
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
  steps_to_inc: int;
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
  | Broken

type vehicle = {
  v_owner_id: int;
  speed : float;
  capacity : int;
  v_t : v_type;
  cargo: good option;
  age: int;
  status: v_status;
  x: float;
  y: float;
  destination: int list;
  v_loc: int option;
}

type connection = {
  c_owner_id: int;
  l_start: int;
  l_end: int;
  length: float;
  c_age: int;
  c_speed: float;
}

module Location : sig
  type t = location
  type label = location
  val equal : t -> t -> bool
  val hash : t -> int
  val compare : t -> t -> int
end

module Connection : sig
  type t = connection
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val default : t
end


module Map : (Graph.Sig.P with type V.t = Location.t and
  type V.label = Location.t and type E.t = Location.t * Connection.t *Location.t
  and type E.label = Connection.t)

type game_state = {
  vehicles : vehicle list;
  graph: Map.t;
  game_age : int;
  paused: bool;
  mutable players : Player.player list;
}

module ConnectionWeight : sig
  type edge = Map.E.t
  type t = float
  val weight : edge -> t
  val compare : t -> t -> int
  val add : t -> t -> t
  val zero : t
end

module Dijkstra : sig
  val shortest_path : Map.t -> Map.V.t -> Map.V.t ->
    (Map.E.t list) * ConnectionWeight.t
end

val fps : float
(*Information with regards to vehicle and road prices.*)
val car_price : float
val truck_price : float
val sell_back_percentage : float
val road_unit_cost : float
val road_length_cost_exponent : float
val road_rights_unit_cost : float
(*Amount of money needed to win the game*)
val win_condition : float
(*Breakdown chance. (Typically set to 0).*)
val breakdown_chance : float
val car_speed : float
val truck_speed : float
val car_capacity : int
val truck_capacity : int
(*Used to determine how prices fluctuate.*)
val price_update_steps : int

(*AI info*)
val ai_max_level : int
val easy_ai_level : int
val medium_ai_level : int
val hard_ai_level : int
val brutal_ai_level : int

(*Returns the cost of the road based on two locations. The cost
 *of the road changes depending on whether it exists.*)
val calculate_buy_road_cost : location -> location -> Map.t -> float

(*Returns the money earned from selling a road*)
val calculate_sell_road_cost : location -> location -> float

(*Returns a location's info based on its ID*)
val get_loc : int -> Map.t -> location

(*Updates vehicle movement*)
val update_vehicles : vehicle list -> Map.t -> player list-> game_state ->
  vehicle list

(*Updates location info (Mainly in terms of price fluctuations *)
val update_locations : Map.t -> int -> Map.t

(*Ensures that all AI have the set difficulty*)
val set_game_difficulty : ai_game_difficulty -> game_state -> game_state