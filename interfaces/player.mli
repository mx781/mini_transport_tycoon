 type p_type =
    | Human
    | AI of int (*AI skill level*)


 type player = {
    p_id : int;
    p_type : p_type;
    money: int;
}


(* Takes in a player and a process and decides if the player is making any
 * changes to the game in this game_step. For human players this is whether
 * or not any valid key or mouse input was received in this game_state, and
 * for AI players this is if they “decide” to make any move in this game_step.
 * After determining if there is a change, update_player evaluates to a
 * game_state representing the updated game after the move, if any, was made.*)
val update: player list -> player list


(*AI function: take_action takes in the game_state; in response to the
  game_state the AI does some particular action*)
val ai_take_action: game_state -> process
