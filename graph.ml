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
                fill_circle loc.x loc.y r
  | Edge road -> set_color black;
                 moveto road.loc1.x road.loc1.y;
                 lineto road.loc2.x road.loc2.y

  let rec draw_all_parts g =
    match g with
    | [] -> ()
    | h::t -> draw_part h; draw_all_parts t

  let draw_graph g =
    open_graph ""; set_window_title "Concept";
    draw_all_parts g;

   (* To run example in utop:

   #use "graph.ml";;
   let give_use_time = "Weird copy and paste functionality for #use in utop";;
   let a = {name="A"; x=40; y=70};;
   let b = {name="B"; x=330; y=170};;
   let c = {name="C"; x=120; y=350};;
   let d = {name="D"; x=340; y=50};;
   let ab = {loc1=a;loc2=b;cost=15};;
   let ac = {loc1=a;loc2=c;cost=20};;
   let ad = {loc1=a;loc2=d;cost=20};;
   let bc = {loc1=b;loc2=c;cost=10};;
   let bd = {loc1=b;loc2=d;cost=10};;
   let cd = {loc1=c;loc2=d;cost=5};;
   let gr = [Node a;Node b;Node c;Node d;
             Edge ab;Edge ac;Edge ad; Edge bc;Edge bd;Edge cd];;
   draw_graph gr;;
   close_graph ();;

   *)