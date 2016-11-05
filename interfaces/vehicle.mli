  type v_type =
    | Car
    | Truck


  type v_status =
    | Waiting
    | Driving
    | Broken (*Equivalent to waiting but inaccessible*)

  type vehicle = {
    v_id: int;
    t : v_type;
    speed : float;
    capacity: int;
    cargo: good; (*For single resource type this will only have one element *)
    age: int; (*In game steps, useful for breakdowns etc.*)
    status: v_status;
    x: int;
    y: int;
  (*Is a list containing the ids of the locations to be traversed in order by the vehicle. The head of the list gives the current destination while the last id gives the final destination. *)
    destination: int list;
    predicted_travel_time : int option (*None if the car is not moving*)
  }


(* Takes in a vehicle and updates the location (if the vehicle is driving) based on the input processes, the age, and possibly the destination list of the vehicle.*)
val update: process list -> vehicle list -> vehicle list


(*Checks the status of a vehicle; returning the destination and the expected run time*)
val check_vehicle_status : vehicle -> unit
