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

(*The AI uses the current state of the game to determine a list of processes
 *to run at any given moment. *)
val make_c_move : game_state -> player_id -> process list

(*Returns a process that initializes a vehicle according to a vehicle type,
 *a player id and a location id*)
val init_vehicle : player_id -> v_type -> loc_id -> Map.t -> process

(*Returns a process corresponding to buying a road for the player
 *corresponding to the two locations in the given map.*)
val buy_road: player_id -> loc_id -> loc_id -> Map.t -> process

(*Same as buying a road, but returns a process for selling one.
 *Uses locations rather than location ids.*)
val sell_road : player_id -> location -> location -> Map.t -> process

(*Returns a process purchasing cargo for a particular player
 *and owned vehicle. *)
val buy_vehicle_cargo : player_id -> vehicle -> r_type -> game_state -> process

(*Returns a process setting a particular vehicle's destination to a particular
 *point. The player id is the first int.*)
val set_vehicle_dest : player_id -> vehicle -> location -> location ->
  game_state -> process

(*Returns a process that refers to selling a particular vehicle*)
val sell_vehicle : player_id -> vehicle -> process