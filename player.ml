type p_type =
    | Human
    | AI of int (*AI skill level*)


 type player = {
    p_id : int;
    p_type : p_type;
    money: float;
  }

type ai_game_difficulty =
  | Easy
  | Medium
  | Hard
  | Brutal

