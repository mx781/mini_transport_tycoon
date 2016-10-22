Module Engine.ml = struct



  type r_type =
    | Lumber
    | Iron
    | Oil
    | Electronics
    | Produce

  type resource = {
    t : r_type;
    quantity : int;
  }

  type location = {
    id: int;
    location: float * float;
    gen_resources: resource list;
    des_resources: (resource*float) list (*float is price*)
  }

  (* Arbitrary ordering means we could make this a list of locations, but it
   * shouldn't hurt to have these in case we ever want directed routes *)
  type connection = {
    id: int;
    l_start: location;
    l_end: location;
    age: int; (*In game steps, useful for breakdowns etc.*)
  }

  type v_type =
    | Car
    | Truck

  type v_status =
    | Waiting
    | Driving
    | Broken

  (*Do we want drivers to be a thing or just more in depth vehicle stats?
   * Not entirely sure of the value drivers add/how that system will look in game.
   * Open to changes here.*)
  type vehicle = {
    id: int;
    t : v_type;
    speed : float;
    capacity: int;
    cargo: resource; (*For single resource type this will only have one element *)
    age: int; (*In game steps, useful for breakdowns etc.*)
    status: v_status;
    location: float * float;
    destination: location;
  }

  type p_type =
    | Human
    | AI of int (*AI skill level*)

  type player = {
    id : int;
    t : p_type;
    money: float;
    vehicles: vehicle list;
    connections: connection list;
  }

  type gamestate = {
    vehicles : vehicle list;
    locations : location list;
    players : player list;
    game_age : int; (*Number of steps since the game began
      (useful for changes that do not happen every frame)*)
  }

  let rec main_loop st =
    Unix.sleepf 0.016;
    print_endline (string_of_int st.game_age);
    let st' = {vehicles = st.vehicles;
              locations = st.locations;
              players = st.players;
              game_age = st.game_age + 1} in
    main_loop st'





end