open Graphics
open GameElements
open Player
open InputProcessing

let default_color = 0xCD853F
let scale =  ref 2 (* No longer supported *)
let button_width = 93
let button_height = 50
let screen_width = 500
let screen_height = 300

(******************************IMAGES*****************************************)

let make_transp img =
  let replace = Array.map (fun col -> if col = white then transp else col) in
  Array.map (fun arr -> replace arr) img

let get_img img =
  Images.load img [] |> Graphic_image.array_of_image |> make_transp

let _ = open_graph ""; set_window_title "Mini Transport Tycoon - by Dan, Jon, Max, and Pat"
(* 8Bit live wallpaper by Nysis*)
let start_screen = get_img "images/start.png" |> make_image
let title_screen = get_img "images/title.png" |> make_image
(* pixel art game cars collection by shutterstock *)
let car_img = get_img "images/car.png" |> make_image
let truck_img = get_img "images/truck.png" |> make_image
(* Buttons *)
let newgame = get_img "images/newgame.png" |> make_image
let loadgame = get_img "images/loadgame.png" |> make_image
let help = get_img "images/help.png" |> make_image
let exit = get_img "images/exit.png" |> make_image
let settings = get_img "images/settings.png" |> make_image

let save = get_img "images/savebutt.png" |> make_image
let pause = get_img "images/pausebutt.png" |> make_image
let buycar = get_img "images/carbutt.png" |> make_image
let buytruck = get_img "images/truckbutt.png" |> make_image
let buyroad = get_img "images/buyroad.png" |> make_image
let sellroad = get_img "images/sellroad.png" |> make_image
let addcargo = get_img "images/addcargo.png" |> make_image
let moveauto = get_img "images/moveauto.png" |> make_image
let sellauto = get_img "images/sellauto.png" |> make_image
let confirm = get_img "images/confirm.png" |> make_image
let cancel = get_img "images/cancel.png" |> make_image
(* Resources *)
let tech = get_img "images/elect.png" |> make_image
let fruit = get_img "images/fruit.png" |> make_image
let oil = get_img "images/oil.png" |> make_image
let drugs = get_img "images/drugs.png" |> make_image
let wood = get_img "images/lumber.png" |> make_image
(* Small Resources *)
let tech_s = get_img "images/elect_s.png" |> make_image
let fruit_s = get_img "images/fruit_s.png" |> make_image
let oil_s = get_img "images/oil_s.png" |> make_image
let drugs_s = get_img "images/drugs_s.png" |> make_image
let wood_s = get_img "images/lumber_s.png" |> make_image
(* No Resources *)
let notech = get_img "images/notech.png" |> make_image
let nofruit = get_img "images/nofruit.png" |> make_image
let nooil = get_img "images/nooil.png" |> make_image
let nodrugs = get_img "images/nodrugs.png" |> make_image
let nowood = get_img "images/nolumber.png" |> make_image

let house = get_img "images/house.png" |> make_image
let bg = get_img "images/bg.png" |> make_image
let gameover = get_img "images/gameover.png" |> make_image

let n1 = get_img "font/1.png" |> make_image
let n2 = get_img "font/2.png" |> make_image
let n3 = get_img "font/3.png" |> make_image
let n4 = get_img "font/4.png" |> make_image
let n5 = get_img "font/5.png" |> make_image
let n6 = get_img "font/6.png" |> make_image
let n7 = get_img "font/7.png" |> make_image
let n8 = get_img "font/8.png" |> make_image
let n9 = get_img "font/9.png" |> make_image
let n0 = get_img "font/0.png" |> make_image
let e = get_img "font/E.png" |> make_image
let p = get_img "font/P.png" |> make_image
let s = get_img "font/S.png" |> make_image
let n = get_img "font/N.png" |> make_image
let i = get_img "font/I.png" |> make_image
let r = get_img "font/R.png" |> make_image
let w = get_img "font/W.png" |> make_image
let dot = get_img "font/dot.png" |> make_image
let colon = get_img "font/colon.png" |> make_image
let dollar = get_img "font/dollar.png" |> make_image

(******************************HELPER FUNCTIONS********************************)

let img_of_str str =
  match str with
  | "E" -> e
  | "I" -> i
  | "N" -> n
  | "P" -> p
  | "R" -> r
  | "S" -> s
  | "W" -> w
  | "1" -> n1
  | "2" -> n2
  | "3" -> n3
  | "4" -> n4
  | "5" -> n5
  | "6" -> n6
  | "7" -> n7
  | "8" -> n8
  | "9" -> n9
  | "0" -> n0
  | "." -> dot
  | ":" -> colon
  | "$" -> dollar
  | _ -> dot

let rtos r =
  match r with
  | Lumber -> "Wood"
  | Iron -> "Drugs"
  | Oil -> "Oil"
  | Electronics -> "Tech"
  | Produce -> "Fruit"

let rec chr_lst str =
  match str with
  | "" ->[]
  | ch ->(String.sub ch 0 1)::(chr_lst (String.sub ch 1 ((String.length ch)-1)))

let rec draw_chars chars x y =
  let width = 16 in
  match chars with
  | [] -> ()
  | " "::t -> draw_chars t (x+width) y
  | h::t -> draw_image (img_of_str h) x y;
            draw_chars t (x+width) y

let draw_str str x y =
  draw_chars (chr_lst str) x y

let round flt =
  int_of_float (flt +. 0.5)

let two_dec flt =
  float_of_int (round (flt *. 100.)) /. 100.

let string_of_float flt =
  let str = string_of_float flt in
  if String.index str '.' = (String.length str - 1) then str ^ "00" else
  if String.index str '.' = (String.length str - 2) then str ^ "0" else
  str

let get_some opt =
  match opt with
  | Some v -> v
  | _ -> failwith "Always check None before using get_some"

let player_color pid =
  match pid with
  | -1 -> default_color
  | 0 -> red
  | 1 -> yellow
  | 2 -> blue
  | 3 -> white
  | 4 -> black
  | _ -> green

(******************************GRAPHICS****************************************)

let draw_start () =
  resize_window 500 500;
  let y = 100 in
  let x = 100 in
  let offset = button_width/2 in
  draw_image bg 0 0;
  draw_image title_screen 0 0;
  draw_image newgame x y;
  draw_image loadgame (x+button_width) y;
  draw_image help (x+2*button_width) y;
  draw_image settings (x+offset) (y-button_height);
  draw_image exit (x+button_width+offset) (y-button_height)

let settings () =
  print_endline "settings";
  let y = 100 in
  let x = 100 in
  draw_image bg 0 0;
  print_endline "drawing";
  draw_image newgame x y;
  draw_image loadgame (x+button_width) y;
  draw_image help (x+2*button_width) y;
  let status = wait_next_event [Button_down] in
  let sx, sy = status.mouse_x, status.mouse_y in
  ()


let rec title_click () =
  draw_start ();
  let status = wait_next_event [Button_down] in
  let b_x = 100 in
  let b_y = 100 in
  let offset = button_width/2 in
  let x = status.mouse_x in
  let y = status.mouse_y in
  if (y < b_y+button_height && y > b_y) then (
    if x < b_x+button_width && x > b_x then 1 else
    if x < b_x+2*button_width && x > b_x+button_width then 2 else
    if x < b_x+3*button_width && x > b_x+button_width then 3 else
    title_click () )
  else if (y < b_y && y > b_y-button_height) then (
    if x < b_x+button_width+offset && x > b_x+offset then 4 else
    if x < b_x+2*button_width+offset && x > b_x+button_width+offset then 5 else
    title_click () )
  else title_click ()


let draw_line ?(color=default_color) ?(width=8) (x1,y1) (x2,y2) =
  set_color color;
  set_line_width (!scale * width);
  moveto x1 y1;
  lineto x2 y2

let draw_ograph grph : unit =
  let label = Map.V.label in
  Map.iter_edges_e
    (fun (v1, e, v2) ->
      let pos1 = ( (round (label v1).l_x)/ 2 * !scale,
                   (round (label v1).l_y)/ 2 * !scale ) in
      let pos2 = ( (round (label v2).l_x)/ 2 * !scale,
                   (round (label v2).l_y)/ 2 * !scale ) in
      draw_line pos1 pos2;
      let pid = e.c_owner_id in
      if pid < 0 then ()
      else draw_line ~color:(player_color pid) ~width:4 pos1 pos2
     )  grph;

  Map.iter_vertex
    (fun v -> let (x,y) = ((Map.V.label v).l_x,
                          (Map.V.label v).l_y) in
     draw_image house
       ((round x - 30)/ 2 * !scale) ((round y - 30)/ 2 * !scale)) grph

let draw_vehicle (v:vehicle) : unit =
  let pic = (match v.v_t with
            | Car -> car_img
            | Truck -> truck_img) in
  let x = round ((v.x -. 30.0) /. 2. *. (float_of_int !scale)) in
  let y = (round ((v.y -. 15.0) /. 2. *. (float_of_int !scale))) in
  Graphics.draw_image pic x y;
  set_color black;
  fill_circle x y 10;
  set_color (player_color v.v_owner_id);
  fill_circle x y 8;
  if v.cargo = None then () else (
    let img = match (get_some v.cargo).t with
              | Electronics -> tech_s
              | Oil -> oil_s
              | Iron -> drugs_s
              | Lumber -> wood_s
              | Produce -> fruit_s in
    draw_image img (x-10) (y+20) )


let rec draw_vehicles (vs:vehicle list) : unit =
  match vs with
  | v::t -> draw_vehicles t; draw_vehicle v
  | [] -> ()

let draw_player_info (p:Player.player) : unit =
  let x = 780 in
  let y = (550-p.p_id*30) in
  set_color black;
  fill_circle (x-10) (y+10) 10;
  set_color (player_color p.p_id);
  fill_circle (x-10) (y+10) 8;
  draw_str ("P"^string_of_int p.p_id^":$"^
            string_of_float(two_dec p.money)) x y
  (*  moveto (p.p_id) (10*p.p_id);
   draw_string ("Player " ^ (string_of_int p.p_id) ^
                ": $" ^(string_of_float p.money)) *)

let rec draw_players (ps:Player.player list) : unit =
  match ps with
  | p::t -> draw_players t; draw_player_info p
  | [] -> ()

let spacing = 25 * !scale
let start_height = 275 * !scale

let draw_buttons () =
  draw_image save 0 start_height;
  draw_image pause 0 (start_height-spacing);
  draw_image buycar 0 (start_height-2*spacing);
  draw_image buytruck 0 (start_height-3*spacing);
  draw_image buyroad 0 (start_height-4*spacing);
  draw_image moveauto 0 (start_height-5*spacing);
  draw_image addcargo 0 (start_height-6*spacing);
  draw_image sellauto 0 (start_height-7*spacing);
  draw_image sellroad 0 (start_height-8*spacing);
  draw_image confirm 0 (start_height-10*spacing);
  draw_image cancel 0 (start_height-11*spacing)

let draw_info_box x y v =
  let box_height = 100 in
  set_color black; fill_rect (x-2) (y-2) 154 (box_height+4);
  set_color white; fill_rect x y 150 box_height;
  let loc = Map.V.label v in
  set_color black;
  (* set_font "-misc-dejavu sans mono-bold-r-normal--256-0-0-0-m-0-iso8859-1";*)
  moveto (x+10) (y+ box_height - 20);
  draw_string "Accepts:";
  rmoveto (-(fst (text_size "Accepts:"))) 0;
  List.iter (fun acc ->
    rmoveto 0 (-12);
    let str = (rtos acc.resource ^": $"^ string_of_float (two_dec acc.price)) in
    draw_string str;
    rmoveto (-fst (text_size str)) 0;
    ) loc.accepts;

  rmoveto 0 (-20);
  draw_string "Produces:";
  rmoveto (-(fst (text_size "Produces:"))) 0;
  List.iter (fun prod ->
    rmoveto 0 (-12);
    let str = string_of_int prod.current ^ " " ^ rtos prod.resource ^
              ": $" ^ string_of_float (two_dec prod.price) in
    draw_string str;
    rmoveto (-fst (text_size str)) 0;
    ) loc.produces

  let draw_hover grph =
  let stat = wait_next_event [Poll;Button_down;Button_up] in
  let x = stat.mouse_x in
  let y = stat.mouse_y in
  let close_enough = 30 in
  let labl = Map.V.label in
  Map.iter_vertex
    (fun v -> let (x1,y1) = (labl v).l_x, (labl v).l_y in
              if (abs (x*2/ !scale - round x1) < close_enough)
              && (abs (y*2/ !scale - round y1) < close_enough)
                             then draw_info_box x y v else ()) grph

let draw_game_state (gs:game_state) : unit =
  draw_image bg 0 0;
  draw_players gs.players;
  draw_ograph gs.graph;
  draw_vehicles gs.vehicles;
  draw_buttons ();
  draw_hover gs.graph


let rec rec_draw_circles p_win gs =
   let col = (player_color p_win) in
   let color = if col = black then white else col in
  (*   draw_str ("P" ^ (string_of_int p_win)) (Random.int screen_width * !scale)
             (Random.int screen_height * !scale);
    draw_str ("P" ^ (string_of_int p_win)) (Random.int screen_width * !scale)
             (Random.int screen_height * !scale);
    draw_str ("P" ^ (string_of_int p_win)) (Random.int screen_width * !scale)
             (Random.int screen_height * !scale);
    draw_str ("P" ^ (string_of_int p_win)) (Random.int screen_width * !scale)
             (Random.int screen_height * !scale); *)
(*     fill_circle (Random.int screen_width * !scale)
                (Random.int screen_height * !scale) 20;
    fill_circle (Random.int screen_width * !scale)
                (Random.int screen_height * !scale) 20; *)
    draw_image truck_img (Random.int screen_width * !scale)
                         (Random.int screen_height * !scale);
    draw_image car_img (Random.int screen_width * !scale)
                       (Random.int screen_height * !scale);
   draw_image house (Random.int screen_width * !scale)
                       (Random.int screen_height * !scale);
(*     draw_image drugs (Random.int screen_width * !scale)
                     (Random.int screen_height * !scale); *)
    set_color black;
    fill_rect (screen_width/2-10) (screen_height-60) 480 170;
    set_color color;
    fill_rect (screen_width/2) (screen_height-50) 460 150;
    draw_image gameover (screen_width/2) (screen_height);
    draw_str ("WINNER IS: P" ^ (string_of_int p_win)) (screen_width/2+120) (screen_height -30);
    set_color black;
    fill_rect 740 370 240 220;
    set_color color;
    fill_rect 750 380 220 200;
    draw_players gs.players;
    Unix.sleepf 0.004;
    rec_draw_circles p_win gs

  let draw_winner p_win gs =
    draw_ograph gs.graph;
    draw_vehicles gs.vehicles;
    rec_draw_circles p_win gs

(******************************INPUT******************************************)

let is_cancelled (x,y) =
  (y < start_height+button_height-11*spacing && y > start_height-11*spacing) ||
  (y < start_height+button_height && y > start_height-1*spacing)

let is_confirmed (x,y) =
  y < start_height+button_height-10*spacing && y > start_height-10*spacing

let rec wait_confirm () =
  let stat = wait_next_event [Button_down] in
  let pos = (stat.mouse_x, stat.mouse_y) in
  if is_cancelled pos || is_confirmed pos
  then is_confirmed pos else wait_confirm ()

let rec get_loc_near ?(l_id = (-1)) ?(click = true) ?(pos = (0,0)) grph =
  let (x,y) = if click then let stat = wait_next_event [Button_down] in
                            (stat.mouse_x, stat.mouse_y)
              else pos in
  let loc = ref None in
  let close_enough = 30 in
  let labl = Map.V.label in
  Map.iter_vertex
    (fun v -> let (x1,y1) = (labl v).l_x, (labl v).l_y in
              if (abs (x*2/ !scale - round x1) < close_enough)
              && (abs (y*2/ !scale - round y1) < close_enough)
                                  then loc := Some v else () ) grph;
  match !loc with
  | Some v when (labl v).l_id <> l_id -> Some v
  | _ -> if is_cancelled (x,y) then None else
         (* print_endline "Not a valid location"; *)
         get_loc_near ~l_id:l_id grph

let rec get_auto_near gs =
  let stat = wait_next_event [Button_down] in
  let (x,y) = (stat.mouse_x, stat.mouse_y) in
  let auto = ref None in
  let close_enough = 30 in
  List.iter
    (fun v -> let (x1,y1) = round v.x, round v.y in
              if (abs (x*2/ !scale - x1) < close_enough)
              && (abs (y*2/ !scale - y1) < close_enough)
              && v.v_owner_id = 0
                                  then auto := Some v else () ) gs.vehicles;
  match !auto with
  | Some v -> Some v
  | None -> if is_cancelled (x,y) then None else
            (* print_endline "No automobile there"; *)
            get_auto_near gs

let get_start_end grph =
  print_endline "Select a start location.";
  let start_loc = get_loc_near grph in
  if start_loc = None then (None, None) else
  (print_endline "Select an end location.";
  let end_loc =
    get_loc_near ~l_id:(Map.V.label (get_some start_loc)).l_id grph in
  (start_loc, end_loc))

let rec pick_cargo loc =
  let res_start = start_height-6*spacing in
  let res_space = 56 in
  let resource_list = List.map (fun prod -> prod.resource) loc.produces in
  if List.exists ((=) Electronics) resource_list
    then draw_image tech button_width res_start
    else draw_image notech button_width res_start;
  if List.exists ((=) Oil) resource_list
    then draw_image oil (button_width+res_space) res_start
    else draw_image nooil (button_width+res_space) res_start;
  if List.exists ((=) Produce) resource_list
    then draw_image fruit (button_width+2*res_space) res_start
    else draw_image nofruit (button_width+2*res_space) res_start;
  if List.exists ((=) Lumber) resource_list
    then draw_image wood (button_width+3*res_space) res_start
    else draw_image nowood (button_width+3*res_space) res_start;
  if List.exists ((=) Iron) resource_list
    then draw_image drugs (button_width+4*res_space) res_start
    else draw_image nodrugs (button_width+4*res_space) res_start;
  let stat = wait_next_event [Button_down] in
  let (x,y) = stat.mouse_x, stat.mouse_y in
  if is_cancelled (x,y) then None else
  if y > res_start+button_height || y < res_start
    then pick_cargo loc else
  let cargo =
  if x > button_width && x < button_width+res_space
    then Some Electronics else
  if x > button_width+res_space && x < button_width+2*res_space
    then Some Oil else
  if x > button_width+2*res_space && x < button_width+3*res_space
    then Some Produce else
  if x > button_width+3*res_space && x < button_width+4*res_space
    then Some Lumber else
  if x > button_width+4*res_space && x < button_width+5*res_space
    then Some Iron
  else
    pick_cargo loc in
  if cargo = None then None else
  if List.exists ((=) (get_some cargo)) resource_list
  then cargo else pick_cargo loc

let quit gs =
  print_endline "Saving game to data/save.json";
  DataProcessing.save_file gs "data/save.json";
  EndGame

let rec pause () =
  let _ = wait_next_event [Button_down] in
  print_endline "Game Paused. Click anywhere to continue\n";
  Pause

let buy_car (gs:game_state) player_id =
  print_endline ("Select a start location." ^ "\nThe car will cost $"
    ^ (string_of_float car_price));
  match get_loc_near gs.graph with
  | None -> (print_endline "Cancelled\n"; Nothing)
  | Some loc ->
  InputProcessing.init_vehicle player_id Car loc.l_id gs.graph

let buy_truck (gs:game_state) player_id =
  print_endline ("Select a start location." ^ "\nThe truck will cost $"
    ^ (string_of_float truck_price));
  match get_loc_near gs.graph with
  | None -> (print_endline "Cancelled\n"; Nothing)
  | Some loc ->
  InputProcessing.init_vehicle player_id Truck loc.l_id gs.graph

let buy_road gs player_id =
  let (start_loc, end_loc) = get_start_end gs.graph in
  if start_loc = None || end_loc = None
  then (print_endline "Cancelled\n"; Nothing) else (
  let cost = calculate_buy_road_cost (get_some start_loc) (get_some end_loc) gs.graph in
  print_endline ("The road will cost $" ^ (string_of_float (two_dec cost)) ^ "\nConfirm to buy.");
  let confirmed = wait_confirm () in
  if (not confirmed) then (print_endline "Cancelled\n"; Nothing) else
  buy_road player_id (get_some start_loc).l_id (get_some end_loc).l_id gs.graph)

let sell_road gs player_id =
  print_endline "Pick two endpoints of the road to sell.";
  let (start_loc, end_loc) = get_start_end gs.graph in
  if start_loc = None || end_loc = None then (print_endline "Cancelled\n"; Nothing) else (
  let cost = calculate_sell_road_cost (get_some start_loc) (get_some end_loc) in
  print_endline ("You will earn $" ^ (string_of_float (two_dec cost)) ^ "\nConfirm to sell.");
  let confirmed = wait_confirm () in
  if not confirmed then (print_endline "Cancelled\n"; Nothing) else
  sell_road player_id (get_some start_loc) (get_some end_loc) gs.graph)

let add_cargo gs player_id =
  print_endline "Pick a vehicle.";
  match get_auto_near gs with
  | None -> (print_endline "Cancelled\n"; Nothing)
  | Some auto when auto.status = Driving ->
    (print_endline "Vehicle must be stopped at a location."; Nothing)
  | Some auto ->
  print_endline "Choose cargo to go in that vehicle.";
  let loc = get_loc_near ~click:false ~pos:(round auto.x, round auto.y) gs.graph in
  if loc = None then Nothing else (
  let cargo = pick_cargo (get_some loc) in
  if cargo = None then Nothing else (
  (* print_endline "That will cost $\nConfirm to buy.";
  let confirmed = wait_confirm () in
  if not confirmed then (print_endline "Cancelled\n"; Nothing) else *)
  buy_vehicle_cargo player_id auto (get_some cargo) gs) )

let move_auto gs player_id =
  print_endline "Pick a vehicle to move.";
  match get_auto_near gs with None -> (print_endline "Cancelled"; Nothing) | Some auto ->
  print_endline "Choose destination.";
  match get_loc_near gs.graph with None -> (print_endline "Cancelled"; Nothing) | Some dest ->
  let l = match auto.v_loc with
    | None -> failwith "vehicle has no location"
    | Some loc -> get_loc loc gs.graph in
  set_vehicle_dest player_id auto l dest gs

let sell_auto (gs:GameElements.game_state) player_id =
  print_endline "Pick a vehicle to sell for\n\tCar: $50.00 \n\tTruck:$100.00";
  match get_auto_near gs with
  | None -> (print_endline "Cancelled\n"; Nothing)
  | Some auto -> sell_vehicle player_id auto


let click_buttons gs player_id =
  let status = wait_next_event [Poll;Button_up;Button_down] in
  let x = status.mouse_x in
  let y = status.mouse_y in
  if not (button_down () && x < button_width && x > 0) then Nothing
  else if x > screen_width || x < 0 then pause ()
  else (
    if y < start_height+button_height
       && y > start_height then quit gs else
    if y < start_height+button_height-spacing
       && y > start_height-spacing then pause () else
    if y < start_height+button_height-2*spacing
      && y > start_height-2*spacing then buy_car gs player_id else
    if y < start_height+button_height-3*spacing
       && y > start_height-3*spacing then buy_truck gs player_id else
    if y < start_height+button_height-4*spacing
       && y > start_height-4*spacing then buy_road gs player_id else
    if y < start_height+button_height-5*spacing
       && y > start_height-5*spacing then move_auto gs player_id else
    if y < start_height+button_height-6*spacing
       && y > start_height-6*spacing then add_cargo gs player_id else
    if y < start_height+button_height-7*spacing
       && y > start_height-7*spacing then sell_auto gs player_id else
    if y < start_height+button_height-8*spacing
       && y > start_height-8*spacing then sell_road gs player_id
    (*Cancel and Confirm don't do anything by itself*)
    else Nothing
  )

