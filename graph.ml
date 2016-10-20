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

  let rec find_loc_near point g' g =
    let close_enough = 25 in
    match g' with
    | [] -> let status = wait_next_event [Button_down] in
            let point' = (status.mouse_x, status.mouse_y) in
            find_loc_near point' g g
    | (Node loc)::_ when (abs (loc.x - (fst point)) < close_enough )
                      && (abs (loc.y - (snd point)) < close_enough ) -> loc
    |  _::t -> find_loc_near point t g

  let make_road loc1 loc2 =
    {loc1=loc1; loc2=loc2; cost = abs (loc1.x-loc2.x + loc1.x-loc2.y)}

  let add_road g =
    let status1 = wait_next_event [Button_down] in
    let point1 = (status1.mouse_x, status1.mouse_y) in
    let loc1 = find_loc_near point1 g g in
    let status2 = wait_next_event [Button_down] in
    let point2 = (status2.mouse_x, status2.mouse_y) in
    let loc2 = find_loc_near point2 g g in
    let road = make_road loc1 loc2 in
    (Edge road)::g

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
   let gr = add_road gr;;
   draw_graph gr;;
   let gr = add_road gr;;
   draw_graph gr;;
   let gr = add_road gr;;
   draw_graph gr;;
   let gr = add_road gr;;
   draw_graph gr;;
   let gr = add_road gr;;
   draw_graph gr;;
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
   *)