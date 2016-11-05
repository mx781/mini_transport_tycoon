  type connection = {
    c_id: int;
    owner: int;
    l_start: location;
    l_end: location;
    age: int; (*In game steps, useful for breakdowns etc.*)
   speed: int -> int (*speed of vehicle on road*)
  }


(*Forms a new connection from one particular location to another particular location*)
val form_connection: location -> location -> Ograph (?)


(* Takes in a connection and updates the connection based on the input processes and the age.*)
val update: process list -> connection list-> connection list
