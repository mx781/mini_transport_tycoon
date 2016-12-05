open GameElements
open Player

type process =
  | BuyVehicle of vehicle
  | SellVehicle of vehicle
  | SetVehicleDestination of vehicle
  | BuyVehicleCargo of vehicle
  | AddRoad of connection
  | DeleteRoad of connection
  | PurchaseRoad of connection
  | Pause
  | EndGame
  | Nothing

(*This uses the current game state to determine how the AI should make a move.
( p_id refers to the id of the AI.
 *It returns a process list that contains the actions that the AI is going
 *to make at a particular turn. It can be empty, in which case the AI
 *makes no moves this turn. *)
val make_c_move : game_state -> player_id -> process list

(* init vehicle creates a vehicle creation process based on the player_id,
 * the type of vehicle v_type, the startig position location's id start_loc_id,
 * and a graph of locations and edges graph *)
val init_vehicle : player_id -> v_type -> loc_id -> Map.t -> process

(* [buy road player_id l1 l2 graph] creates an AddRoad or PurchaseRoad
 * process from a player_id of the purchaser, the location ids of the start
 * and end points, and the graph of connections and locations. It returns a
 * Nothing process if the road cannot be purchased.
 *)
val buy_road: player_id -> location -> location -> Map.t -> process

(* [sell_road player_id l1 l2 graph] creates a SellRoad
 * process from a player_id of the seller, the location ids of the start
 * and end points, and the graph of connections and locations. It returns a
 * Nothing process if the road cannot be sold.
 *)
val sell_road : player_id -> location -> location -> Map.t -> process

(* [buy_vehicle_cargo player_id v_old r_type st] creates a BuyVehicle process
 * based on the player_id of the purchaser, the old vehicle who will have the
 * cargo added to it v_old, the resource type to buy r_type, and the game state
 * st. It returns a Nothing process if the cargo cannot be purchased.*)
val buy_vehicle_cargo : player_id -> vehicle -> r_type -> game_state -> process

(* [set_vehicle_dest player_id v_old start_loc end_loc st] creates a
 * SetVehicleDestination process from a player_id of the seller, the old vehicle
 * that we are setting the destination of v_old, the starting location start_loc
 * and the ending location end_loc. It returns a Nothing process if the vehicle
 * cannot be routed.
 *)
val set_vehicle_dest : player_id -> vehicle -> location -> location ->
  game_state -> process

(* [sell_vehicle player_id v] creates a SellVehicle
 * process from a player_id of the seller, and the vehicle v to sell. It returns
 * a Nothing process if the road cannot be sold.
 *)
val sell_vehicle : player_id -> vehicle -> process