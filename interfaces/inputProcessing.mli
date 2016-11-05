type process =
  | AddVehicle of int * int
  | CheckLocation of int * int
  | AddRoad of int * int
  | Pause
  | EndGame
  | CheckVehicleStatus of int * int
  | None (*If the action done is irrelevant*)


(*Processes_input (master). Using the information from the input it returns a relevant process that is used to update whatever needs to be updated. The actual updating is done in the main loop.*)
val process_input : status -> process


(*Pauses the game*)
val pause: unit -> unit


(*Ends the game*)
val end_game: unit -> unit
