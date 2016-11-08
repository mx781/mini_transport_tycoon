type r_type =
    | Lumber
    | Iron
    | Oil
    | Electronics
    | Produce


type good = {
  t : r_type;
  quantity : int;
  origin : location
}

type goods_profile = {
  resource: r_type;
  steps_to_inc: int; (*how many game_steps before incrementing current by 1*)
  current: int;
  capacity: int;
  price: int;
  natural_price: int;
}

type location = {
  x: int;
  y: int;
  accepts: goods_profile list;
  produces: goods_profile list;
}

type v_type =
  | Car
  | Truck


type v_status =
  | Waiting
  | Driving
  | Broken (*Equivalent to waiting but inaccessible*)

type vehicle = {
  v_owner_id: int;
  t : v_type;
  speed : float;
  capacity: int;
  cargo: good; (*For single resource type this will only have one element *)
  age: int; (*In game steps, useful for breakdowns etc.*)
  status: v_status;
  x: int;
  y: int;
(* Is a list containing the ids of the locations to be traversed in order by the
 * vehicle. The head of the list gives the current destination while the last id
 * gives the final destination. *)
  destination: int list;
  predicted_travel_time : int option (*None if the car is not moving*)
}

type connection = {
  c_owner_id: int;
  l_start: location;
  l_end: location;
  age: int; (*In game steps, useful for breakdowns etc.*)
  speed: int -> int (*speed of vehicle on road*)
}


(* Forms a new connection from one particular location to
 * another particular location*)
val form_connection: location -> location -> Ograph (?)


(* Takes in a connection and updates the connection based on the input
 * processes and the age.*)
val update: process list -> connection list-> connection list


(* Takes in a vehicle and updates the location (if the vehicle is driving) based
 * on the input processes, the age, and possibly the destination list of the
 * vehicle.*)
val update: process list -> vehicle list -> vehicle list


(* Checks the status of a vehicle; returning the destination and the
 * expected run time*)
val check_vehicle_status : vehicle -> unit


(* Takes in a list of processes and a location within the
 * gameworld and evaluates to the same location updated by one game_step,
 * based on the input processes. Here “update” means generating available
 * resources, fluctuating the asking and selling price for each resource, a
 * and fluctuating the amount of a resource demanded. *)
val update: process list -> location -> location


(* Returns the characteristics associated with a given location (such as the
 * goods sold and the prices of those goods. These characteristics should
 * show up on the screen.*)
val location_info: location -> unit

