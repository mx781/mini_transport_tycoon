 open Graph
 open GameElements
 open Player
 open InputProcessing
 open DataProcessing

(*Runs the main loop of the game*)
val main_loop : game_state -> unit

(*Displays the title screen for the game*)
val title_screen : ai_game_difficulty -> unit

(*Does the process handling. The boolean is for checking if a road has been
 *bought (and is used to make certain operations easier.*)
val handle_processes : process list -> game_state -> bool -> game_state

