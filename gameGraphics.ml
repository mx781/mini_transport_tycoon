open Graphics
open GameElements
open Player

let vertex_color = black
let default_color = 0xCD853F
let vertex_size = 5
let scale =  ref 2

let make_transp img =
  let replace = Array.map (fun col -> if col = white then transp else col) in
  Array.map (fun arr -> replace arr) img

let get_img img =
  Images.load img [] |> Graphic_image.array_of_image |> make_transp

let _ = open_graph " 380x380"
(* 8Bit live wallpaper by Nysis*)
let start_screen = get_img "images/start.png" |> make_image
(* pixel art game cars collection by shutterstock *)
let car_img = get_img "images/car.png" |> make_image
let truck_img = get_img "images/truck.png" |> make_image
let save = get_img "images/savebutt.png" |> make_image
let pause = get_img "images/pausebutt.png" |> make_image
let buycar = get_img "images/carbutt.png" |> make_image
let buytruck = get_img "images/truckbutt.png" |> make_image
let house = get_img "images/house.png" |> make_image
let bg = get_img "images/bg.png" |> make_image

let draw_start () =
  draw_image start_screen 0 0

let round flt =
  int_of_float (flt +. 0.5)

let open_screen size =
  scale := (int_of_string size);
  let display = string_of_int (400 * !scale) ^ "x" ^ string_of_int (300 * !scale) in
  (* open_graph (" " ^ display) *)
  resize_window (400* !scale) (300* !scale)


let draw_line ?(color=default_color) ?(width=8) (x1,y1) (x2,y2) =
  set_color color;
  set_line_width (!scale * width);
  moveto x1 y1;
  lineto x2 y2

let draw_ograph grph : unit =
  GameElements.Map.iter_edges
    (fun v1 v2 -> draw_line
      ((round ((GameElements.Map.V.label v1).l_x) / 2 * !scale),
      (round ((GameElements.Map.V.label v1).l_y)/ 2 * !scale))
      ((round ((GameElements.Map.V.label v2).l_x)/ 2 * !scale),
      (round ((GameElements.Map.V.label v2).l_y)/ 2 * !scale))
     )  grph;

  set_color vertex_color;
  GameElements.Map.iter_vertex
    (fun v -> let (x,y) = ((GameElements.Map.V.label v).l_x,
                          (GameElements.Map.V.label v).l_y) in
    (* fill_circle ((round x)/ 2 * !scale) ((round y)/ 2 * !scale) (!scale * vertex_size)) grph *)
       draw_image house ((round x - 30)/ 2 * !scale) ((round y - 30)/ 2 * !scale)) grph

let draw_vehicle (v:GameElements.vehicle) : unit =
  let pic = (match v.t with
            | GameElements.Car -> car_img
            | GameElements.Truck -> truck_img) in
  let x = (round (v.x -. 30.0 *. (float_of_int !scale)/.2.)) in
  let y = (round (v.y -. 15.0 *. (float_of_int !scale)/.2.)) in
  Graphics.draw_image pic x y

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

let button_size = open_graph ""; 50

let _ = close_graph


let draw_buttons () =

  let spacing = 55 in
  let start_height = 250 * !scale in
  draw_image (save ) 0 start_height;
  draw_image (pause ) 0 (start_height-spacing);
  draw_image (buycar ) 0 (start_height-2*spacing);
  draw_image (buytruck) 0 (start_height-3*spacing)
(*   moveto (!scale*10) (!scale*110);
  draw_string "Buy Car";
  draw_rect (!scale*0) (!scale*100) (!scale*button_size) (!scale*button_size);
  moveto (!scale*10) (!scale*160);
  draw_string "Buy Truck";
  draw_rect (!scale*0) (!scale*150) (!scale*button_size) (!scale*button_size);
  moveto (!scale*10) (!scale*210);
  draw_string "Pause";
  draw_rect (!scale*0) (!scale*200) (!scale*button_size) (!scale*button_size);
  moveto (!scale*10) (!scale*260);
  draw_string "Save and Quit";
  draw_rect (!scale*0) (!scale*250) (!scale*button_size) (!scale*button_size) *)



let draw_game_state gs : unit =
  (* clear_graph (); *)
  draw_image (bg)  0 0;
  draw_players gs.players;
  draw_ograph gs.graph;
  draw_vehicles gs.vehicles;
  draw_buttons ();