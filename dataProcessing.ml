(* open Gamestate_piqi *)
open Gamestate_piqi_ext
open GameElements
(* open_inn Piqirun_custom *)

(* [parse_json_file file] parses a valid [file].json into a piqi gamestate. *)
let load_json file =
  let ch = open_in file in
  let rec read_line ch str =
    try
      match input_line ch with
      | s -> read_line ch (str ^ s)
    with
      | End_of_file -> str
  in
  let str = read_line ch "" in
  close_in ch;
  parse_gamestate str `json

(* [convert_gamestate piqi_gs] parses piqi gamestate [piqi_gs] into the native
 * game_state type. *)
let gs_from_piqi piqi_gs =
  let open Gamestate_piqi.Gamestate in
  {
    vehicles = piqi_gs.vehicles;
    graph = piqi_gs.graph;
    players = piqi_gs.players;
    paused = piqi_gs.paused;
    game_age = piqi_gs.gameage;
    ai_info = ref None;
  }

(* TODO: see if we can resolve the necessity to have explicit typing *)
let gs_to_piqi: game_state -> Gamestate_piqi.Gamestate.t = (fun gs ->
  let open Gamestate_piqi.Gamestate in 
  (* print_int (List.length gs.vehicles); *)
  {
    vehicles = gs.vehicles;
    graph = gs.graph;
    players = gs.players;
    paused = gs.paused;
    gameage = gs.game_age;
    aiinfo = `none;
  }
)

let load_file file =
  try
    let piqi_gs = load_json file in
    gs_from_piqi piqi_gs
  with
    | _ -> failwith ("Failed to load " ^ file)

let save_file game_state file =
  try
    let piqi_gs = gs_to_piqi game_state in
    let out_chan = open_out_bin file in
    let json_data = gen_gamestate piqi_gs `json in
    output_string out_chan json_data;
    close_out out_chan;
    print_endline (" [✔] Save successful [" ^ file ^ "]")
  with
  | _ -> failwith (" [✖] Failed to save to [" ^ file ^ "]")