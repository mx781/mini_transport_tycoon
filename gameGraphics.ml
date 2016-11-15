open Graphics
open GameElements
open Player


let vertex_size = 10
let car_size = 5

let open_screen () = open_graph ""

let draw_line ?(color=black) ?(width=3) (x1,y1) (x2,y2) =
  set_color color;
  set_line_width width;
  moveto x1 y1;
  lineto x2 y2

let draw_ograph grph : unit =
  GameElements.Map.iter_vertex
    (fun v -> let (x,y) = ((GameElements.Map.V.label v).l_x, (GameElements.Map.V.label v).l_y) in fill_circle (int_of_float x) (int_of_float y) vertex_size) grph;
  GameElements.Map.iter_edges
    (fun v1 v2 -> draw_line
      (int_of_float ((GameElements.Map.V.label v1).l_x),
      int_of_float ((GameElements.Map.V.label v1).l_y))
      (int_of_float ((GameElements.Map.V.label v2).l_x),
      int_of_float ((GameElements.Map.V.label v2).l_y))
     )  grph


(* let draw_edge ?(color=black) ?(width=3) (x1,y1) (x2,y2) =
  set_color color;
  set_line_width width;
  moveto x1 y1;
  lineto x2 y2

let draw_vertex ?(color=black) ?(radius=10) (x,y) =
  set_color color;
  fill_circle x y radius *)

let draw_vehicle (v:GameElements.vehicle) : unit =
  fill_circle (int_of_float v.x) (int_of_float v.y) car_size

let rec draw_vehicles (vs:GameElements.vehicle list) : unit =
  match vs with
  | v::t -> draw_vehicles t; draw_vehicle v
  | [] -> ()

let draw_player_info (p:Player.player) : unit =
   moveto (10*p.p_id) (10*p.p_id);
   draw_string ("Player " ^ (string_of_int p.p_id) ^
                ": $" ^(string_of_int p.money))

let rec draw_players (ps:Player.player list) : unit =
  match ps with
  | p::t -> draw_players t; draw_player_info p
  | [] -> ()

let draw_game_state gs : unit =
  clear_graph ();
  draw_ograph gs.graph;
  draw_vehicles gs.vehicles;
  draw_players gs.players