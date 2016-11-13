open GameElements

type process =
  | CheckVehicleStatus of vehicles
  | BuyVehicle of vehicle
  | SellVehicle of vehicle
  | SetVehicleDestination of vehicle (*with updated destinations*)
  | CheckLocation of location
  | AddRoad of connection
  | DeleteRoad of connection
  | PurchaseRoad of connection (*Purchase rights to a road that is preexisting*)
  | CheckRoad of connection
  | Pause
  | EndGame
  | None


(* Processes_input (master). Using the information from the input it returns a
 * relevant process that is used to update whatever needs to be updated.
 * The actual updating is done in the main loop.*)
val process_input : status -> process

(*Pauses the game*)
val pause: unit -> unit

(*Ends the game*)
val end_game: unit -> unit
