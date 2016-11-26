 type p_type =
    | Human
    | AI of int (*AI skill level*)


 type player = {
    p_id : int;
    p_type : p_type;
    money: float;
}

let l_update p_lst =
  p_lst



