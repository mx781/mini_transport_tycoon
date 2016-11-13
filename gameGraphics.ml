open Graphics

let vertex_size = 10;
let car_size = 5;

let draw_arrow ?(color=black) ?(width=3) (x1,y1) (x2,y2) =
  set_color color;
  set_line_width width;
  moveto x1 y1;
  lineto x2 y2

let draw_ograph grph : unit =
  G.iter_vertex
    (fun v -> let (x,y) = G.V.label v in fill_circle x y vertex_size) grph;
  G.iter_edges
    (fun v1 v2 -> draw_line (G.V.label v1) (G.V.label v2)) grph;

(* let draw_edge ?(color=black) ?(width=3) (x1,y1) (x2,y2) =
  set_color color;
  set_line_width width;
  moveto x1 y1;
  lineto x2 y2

let draw_vertex ?(color=black) ?(radius=10) (x,y) =
  set_color color;
  fill_circle x y radius *)

let draw_vehicle (v:vehicle) : unit =
  fill_circle v.x v.y car_size

let rec draw_vehicles (vs:vehicle list) : unit =
  match vs with
  | v::t -> draw_vehicles t; draw_vehicle v
  | [] -> ()

let draw_player_info (p:Player.player) : unit =
   moveto (10*p.p_id) (10*p.p_id);
   draw_string ("Player " ^ (string_of_int p.p_id) ^
                ": $" ^(string_of_int p.money))

let rec draw_players (ps:Player.player) : unit =
  match ps with
  | p::t -> draw_players t; draw_player_info p
  | [] -> ()

let draw_game_state (gs:Engine.gamestate) : unit =
  clear_graph ();
  draw_ograph gs.graph;
  draw_vehicles gs.vehicles;
  draw_score gs.score;
  draw_players gs.players