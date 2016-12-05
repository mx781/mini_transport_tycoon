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

(*The module pertaining to the graph containing locations as vertices and
 *a triple containing two locations and a connection as an edge*)
module Map : (Graph.Sig.P with type V.t = Location.t and
  type V.label = Location.t and type E.t = Location.t * Connection.t *Location.t
  and type E.label = Connection.t)

(*All the necessary information pertaining to the game*)
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


(* [calculate_buy_road_cost start_l end_l] returns the money that a player will
 * pay to buy a road with start location start_l and end location end_l in
 * graph graph, whether this would involve building it from scratch or buying it
 * if it is public.*)
val calculate_buy_road_cost : location -> location -> Map.t -> float

(* [calculate_sell_road_cost start_l end_l] returns the money that a player will
 * earn from selling a given road with start location start_l and end location
 * end_l*)
val calculate_sell_road_cost : location -> location -> float

(*Returns a location's info based on its ID given a graph.*)
(*Precondition: the graph contains the location ID *)
val get_loc : int -> Map.t -> location

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
val update_vehicles : vehicle list -> Map.t -> player list-> game_state ->
  vehicle list

(* [update_location age l] takes in an individual age (game_age) and location l
 * and updates the location accordingly (i.e. their good profiles)*)
val update_locations : Map.t -> int -> Map.t

(* [set_game_difficulty gd st] sets the game difficulty based on a game
 * difficulty gd and a game state st*)
val set_game_difficulty : ai_game_difficulty -> game_state -> game_state