(* Takes in a game_state and a list of processes and updates the display on the
 * screen to draw the background, the menus, the vehicles, roads, locations,
 * requested information, etc. This function
 * evaluates to a unit because it is only drawing to the screen- it cannot
 * change any values in the game_state. *)
val update_display: game_state -> process list -> unit
