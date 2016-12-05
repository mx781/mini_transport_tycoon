 open Graph
 open GameElements
 open Player
 open InputProcessing
 open DataProcessing

(* pre: st is a valid game state, that is, there are no duplicate
 *      location or player ids in the player list or graph.
 * post: [main_loop st] will return the game state of the game when it is
 *       eventually exited. It is our main repl loop and ordinarily recursively
 *       calls itself to calculate and update each frame.
 *)
val main_loop : game_state -> unit

(* pre: dif is an AI difficulty (Easy, Medium, Hard, or Brutal)
 * post: [title_screen dif] displays the title screen and just returns a unit.
 *)
val title_screen : ai_game_difficulty -> unit

(* pre: proclist is any list of valid processes (that is, they must be of the
 *      format such that they are actionable based on the current game state st.
 *      Actionable is defined here as conforming to the rules in the formal
 *      instructions provided in the separate instructions document.
 *      st is a valid game state, that is, there are no duplicate
 *      location or player ids in the player list or graph.
 *      road_bought is true if a road has been bought in this frame by any
 *      player in this frame of the game and false otherwise.
 * post: [handle_processes proclist st road_bought] returns a new game state
 *       updated to reflect all changes that these processes, once handled, will
 *       cause in the game state. However, it will only allow one road to be
 *       purchased for frame to avoid same-frame action conflicts.
 *)
val handle_processes : process list -> game_state -> bool -> game_state

