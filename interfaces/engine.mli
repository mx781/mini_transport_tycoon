 open OGraph

  type game_state = {
    vehicles : vehicle list;
    (* graph constructed with [connection] as edge and Location.location as nodes *)
    graph: OGraph.t;
    players : player list;
    game_age : int; (*Number of steps since the game began
      (useful for changes that do not happen every frame)*)
    paused: bool;
    score: int
  }


(*Takes in a string representing the file name of the game to be loaded and played, loads the file to obtain the corresponding game_state, and enters the main loop to begin playing the game. *)
Init_game: string -> ()


(*main_loop is the main REPL for the game. It takes in a game_state and updates everything within the game_state (location, vehicles, players) so that the game updates by one game_step, evaluating to the updated game_state.*)
val main_loop: game_state -> game_state


