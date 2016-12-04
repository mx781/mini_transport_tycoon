open Gamestate_piqi_ext
open GameElements


(*Takes in a file name referring to a .json file and converts the information
 *within the file*)
val load_file : string -> game_state

(*Uses a game state and stores the information in a json file with the name
 * given by the user*)
val save_file : game_state -> string -> unit