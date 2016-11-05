type location = {
    l_id: int;
    x: int;
    y: int;
    accepts: goods_profile list;
    produces: goods_profile list;
  }


(*Takes in a list of processes and a location within the gameworld and evaluates to the same location updated by one game_step, based on the input processes. Here “update” means generating available resources, fluctuating the asking and selling price for each resource, and fluctuating the amount of a resource demanded. *)
val update: process list -> location -> location


(*Returns the characteristics associated with a given location (such as the goods sold and the prices of those goods. These characteristics should show up on the screen.*)
val location_info: location -> unit

