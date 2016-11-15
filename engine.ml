(*

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
    l_id: int;
    location: float * float;
    gen_resources: resource list;
    des_resources: (resource*float*float) list
    (*snd des_resources is current price, last float is "natural" price
     * (the natural price increases strategy element)*)
  }

  (* Arbitrary ordering means we could make this a list of locations, but it
   * shouldn't hurt to have these in case we ever want directed routes *)
  type connection = {
    c_id: int;
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
    v_id: int;
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
    p_id : int;
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

  let update_price (rsrc,p,np) =
    let p' = if (Random.float 60.0 > 1.0)
    then p
    else
    let () = print_endline (string_of_float p) in
    if (Random.float 1.0 <  0.5 +. (0.25*.(1.0 -. (p/.np)))) || p <= (np/.15.0)
         then p +. np/.(Random.float 100.0 +. 15.0)
         else p -. np/.(Random.float 100.0 +. 15.0)
    in
    (rsrc,p',np)



  let update_location loc =
    {
     l_id = loc.l_id;
     location = loc.location;
     gen_resources = loc.gen_resources;
     des_resources = List.map update_price loc.des_resources;
    }


  let rec main_loop st =
    Unix.sleepf 0.016;
  (*   print_endline (string_of_int st.game_age); *)
    let new_locations = List.map update_location st.locations in
    let st' = {vehicles = st.vehicles;
              locations = new_locations;
              players = st.players;
              game_age = st.game_age + 1} in
    main_loop st'

  let testing_state =
    {vehicles = [];
     locations = [{l_id = 1; location = (0.4,6.5); gen_resources = []; des_resources = [({t = Lumber; quantity = 10},80.0,80.0)]}];
     players = [];
     game_age = 0}

 *)

 open Graph
 open GameElements
 open Player

let prob = 0.0
let new_graph () =
  let v1 = {l_id = 0;
  loc = (45,345);
  accepts= [];
  produces= [];} in
  let v2 = {l_id = 1;
  loc = (455,41);
  accepts= [];
  produces= [];} in
  let m1 = Map.add_vertex (Map.empty) (v1) in
  let m2 = Map.add_vertex m1 (v2) in
  let m3 = Map.add_edge_e m2
  (v1,{
    c_owner_id = 4;
    l_start= 1;
    l_end =  2;
    length= 2;
    age= 0;
    speed= 3;
  },v2) in m3

let fps = 30.0

let rec main_loop st =
  let start_t = Sys.time () in
  GameGraphics.draw_game_state st;
  let processes = [] in
  let new_vehicles = update_vehicles st.vehicles in
  let st' = { vehicles = new_vehicles;
              graph = st.graph;
              players = st.players;
              paused = st.paused;
              game_age = st.game_age + 1;} in
  let time_elapsed = Sys.time () -. start_t in
  print_endline (string_of_float time_elapsed);
  let sleep_time = if ((1.0 /. fps) -. time_elapsed) > 0.0 then ((1.0 /. fps) -. time_elapsed) else 0.0 in
  print_endline (string_of_float sleep_time);
  Unix.sleepf sleep_time;
  main_loop st'


let init_game fname =
  GameGraphics.open_screen ();
  main_loop
  { vehicles=
  [{v_owner_id= 2;
  t = Car;
  speed = 10.0;
  capacity= 100;
  cargo= {
  t = Oil;
  quantity = 5;
  };
  age= 56;
  status= Driving;
  x= 224;
  y= 122;
  destination= [5];
  predicted_travel_time = None}];
  graph = new_graph () ; players = [];
    game_age = 0; paused = false;}
