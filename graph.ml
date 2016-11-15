#load "graphics.cma"
open Graphics

type location =
  {name:string; x:int; y:int}

type road =
  {loc1:location; loc2:location; cost:int}

type part =
  | Node of location
  | Edge of road

type graph =
  part list

let draw_part p =
  let r = 20 in
  match p with
  | Node loc -> set_color blue;
                fill_circle loc.x loc.y r;
                set_color red;
                set_text_size 500;
                moveto (loc.x+r) (loc.y+r);
                draw_string loc.name
  | Edge road -> set_color black;
                 moveto road.loc1.x road.loc1.y;
                 lineto road.loc2.x road.loc2.y;
                 moveto ((road.loc1.x+road.loc2.x)/2)
                        ((road.loc2.y+road.loc1.y)/2);
                 draw_string (string_of_int road.cost)

  let rec draw_all_parts g =
    match g with
    | [] -> ()
    | h::t -> draw_part h; draw_all_parts t

  let draw_graph g =
    open_graph ""; set_window_title "Concept";
    draw_all_parts g

  (* Use to compare Location types *)
  let loc_equal loc1 loc2 =
    String.equal loc1.name loc2.name

  (* Find a Location within a certain tolerance from its position *)
  let rec find_loc_near point g' g =
    let close_enough = 25 in
    match g' with
    | [] -> raise Not_found
    | (Node loc)::_ when (abs (loc.x - (fst point)) < close_enough )
                      && (abs (loc.y - (snd point)) < close_enough ) -> loc
    |  _::t -> find_loc_near point t g

  (* Create a Road between two Locations *)
  let make_road loc1 loc2 =
    {loc1=loc1; loc2=loc2; cost = abs (loc1.x-loc2.x + loc1.x-loc2.y)}

  (* Gets a Location from a mouse event *)
  let rec get_loc g status =
    let point = (status.mouse_x, status.mouse_y) in
      find_loc_near point g g

  (* Gets a Location different from [loc] *)
  let rec get_dif_loc loc g status =
    let new_loc = get_loc g status in
    if loc_equal new_loc loc then get_dif_loc loc g status else new_loc

  (* Add a Road to the graph [g] *)
  let add_road g status =
    let loc1 = get_loc g status in
    let loc2 = get_dif_loc loc1 g status in
    let road = make_road loc1 loc2 in
    (Edge road)::g

  type car = {x:int; y:int; speed:float}

  let draw_car car =
    fill_circle car.x car.y 5

  let rec main_loop g status =
    try
      let status = wait_next_event [Poll;Button_down;Button_up] in
      if button_down () then
        let point = (status.mouse_x, status.mouse_y) in
        let g' = add_road g status in
        draw_graph g';
        Unix.sleepf 2.5;
        (* main_loop g' *)
      else
        Unix.sleepf 2.5;
       (*  main_loop g *)
    with
    | Not_found -> main_loop g


  let animate g g' =
    draw_graph g';
    Unix.sleepf 0.5;
    draw_graph g';
    Unix.sleepf 0.5;
    draw_graph g;
    Unix.sleepf 0.4;
    draw_graph g';
    Unix.sleepf 0.4;
    draw_graph g;
    Unix.sleepf 0.3;
    draw_graph g';
    Unix.sleepf 0.3;
    draw_graph g

   (* To run example in utop, copy and paste, then click on nodes:

   #use "graph.ml";;
   let give_use_time = "Weird copy and paste functionality for #use in utop";;
   let a = {name="A"; x=40; y=70};;
   let b = {name="B"; x=330; y=170};;
   let c = {name="C"; x=120; y=350};;
   let d = {name="D"; x=340; y=50};;
   let e = {name="E"; x=200;y=400};;
   let gr = [Node a;Node b;Node c;Node d; Node e];;
   draw_graph gr;;
   main_loop gr;;
   close_graph ();;

   *)



   (*  --Archive--
   let ab = {loc1=a;loc2=b;cost=15};;
   let ac = {loc1=a;loc2=c;cost=20};;
   let ad = {loc1=a;loc2=d;cost=20};;
   let ad = {loc1=a;loc2=d;cost=20};;
   let ad = {loc1=a;loc2=e;cost=50};;
   let bc = {loc1=b;loc2=c;cost=10};;
   let bd = {loc1=b;loc2=d;cost=10};;
   let be = {loc1=b;loc2=e;cost=10};;
   let ae = {loc1=b;loc2=e;cost=25};;
   let cd = {loc1=c;loc2=d;cost=5};;
   let ce = {loc1=c;loc2=e;cost=10};;
   let de = {loc1=d;loc2=e;cost=15};;
    let gr = [Node a;Node b;Node c;Node d; Node e;
             Edge ab;Edge ac;Edge ad; Edge bc;Edge bd;Edge cd;
             Edge ae; Edge be; Edge ce; Edge de];;
   let gr' = [Node a;Node b;Node c;Node d; Node e;
             Edge ac;Edge ad; Edge bc;Edge bd;Edge cd;
             Edge ae; Edge be; Edge ce; Edge de];;


             let pos = mouse_pos () in
      moveto (fst pos) (snd pos);
      set_color black;
      print_string "LOOP";
      Unix.sleepf 0.2;
      let pos = mouse_pos () in
      lineto (fst pos) (snd pos);
   *)