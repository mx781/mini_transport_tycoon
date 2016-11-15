open Player

type r_type

type good

type goods_profile

type location

type v_type

type v_status

type vehicle

type connection

(* Forms a new connection from one particular location to
 * another particular location*)
(* val form_connection: player -> location -> location -> Map.t
 *)

(* Takes in a connection and updates the connection based on the input
 * processes and the age.*)
val update_connections: connection -> connection list-> connection list



(* Takes in a vehicle and updates the location (if the vehicle is driving) based
 * on the input processes, the age, and possibly the destination list of the
 * vehicle.*)
val update_vehicles: vehicle list -> vehicle list



(* Checks the status of a vehicle; returning the destination and the
 * expected run time*)
val check_vehicle_status : vehicle -> unit


(* Takes in a list of processes and a location within the
 * gameworld and evaluates to the same location updated by one game_step,
 * based on the input processes. Here “update” means generating available
 * resources, fluctuating the asking and selling price for each resource, a
 * and fluctuating the amount of a resource demanded. *)
val update_locations: location list -> location



(* Returns the characteristics associated with a given location (such as the
 * goods sold and the prices of those goods. These characteristics should
 * show up on the screen.*)
val location_info: location -> unit
