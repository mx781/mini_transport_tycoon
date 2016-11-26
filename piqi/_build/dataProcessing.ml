(* open Gamestate_piqi *)
open Gamestate_piqi_ext
open GameElements
(* open_inn Piqirun_custom *)

(* [parse_json_file file] parses a valid [file].json into a piqi gamestate. *)
let parse_json_file file =
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

(* [convert_gamestate piqi_gs] parses a piqi gamestate into the native
 * game_state type. *)
let convert_gamestate piqi_gs =
  let open Gamestate_piqi.Gamestate in
  {
    vehicles = piqi_gs.vehicles;
    graph = piqi_gs.graph;
    players = piqi_gs.players;
    paused = piqi_gs.paused;
    game_age = piqi_gs.gameage;
    ai_info = None
  }