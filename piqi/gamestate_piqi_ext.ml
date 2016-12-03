module Player_ext = Player_piqi_ext
module Graph_ext = Graph_piqi_ext
module Gameelements_ext = Gameelements_piqi_ext
let piqi = Gamestate_piqi.piqi


let _ = Piqirun_ext.init_piqi piqi


let _vehicle_piqi_type = Piqirun_ext.find_piqi_type "gamestate/vehicle"
let _player_piqi_type = Piqirun_ext.find_piqi_type "gamestate/player"
let _graph_piqi_type = Piqirun_ext.find_piqi_type "gamestate/graph"
let _vehiclelist_piqi_type = Piqirun_ext.find_piqi_type "gamestate/vehiclelist"
let _playerlist_piqi_type = Piqirun_ext.find_piqi_type "gamestate/playerlist"
let _gamestate_piqi_type = Piqirun_ext.find_piqi_type "gamestate/gamestate"


let parse_vehicle ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _vehicle_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Gamestate_piqi.parse_vehicle buf

let parse_player ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _player_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Gamestate_piqi.parse_player buf

let parse_graph ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _graph_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Gamestate_piqi.parse_graph buf

let parse_vehiclelist ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _vehiclelist_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Gamestate_piqi.parse_vehiclelist buf

let parse_playerlist ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _playerlist_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Gamestate_piqi.parse_playerlist buf

let parse_gamestate ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _gamestate_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Gamestate_piqi.parse_gamestate buf


let gen_vehicle ?opts x (format :Piqirun_ext.output_format) =
  let buf = Gamestate_piqi.gen_vehicle x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _vehicle_piqi_type `pb format x_pb ?opts

let gen_player ?opts x (format :Piqirun_ext.output_format) =
  let buf = Gamestate_piqi.gen_player x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _player_piqi_type `pb format x_pb ?opts

let gen_graph ?opts x (format :Piqirun_ext.output_format) =
  let buf = Gamestate_piqi.gen_graph x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _graph_piqi_type `pb format x_pb ?opts

let gen_vehiclelist ?opts x (format :Piqirun_ext.output_format) =
  let buf = Gamestate_piqi.gen_vehiclelist x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _vehiclelist_piqi_type `pb format x_pb ?opts

let gen_playerlist ?opts x (format :Piqirun_ext.output_format) =
  let buf = Gamestate_piqi.gen_playerlist x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _playerlist_piqi_type `pb format x_pb ?opts

let gen_gamestate ?opts x (format :Piqirun_ext.output_format) =
  let buf = Gamestate_piqi.gen_gamestate x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _gamestate_piqi_type `pb format x_pb ?opts


let print_vehicle ?opts x =
  Pervasives.print_endline (gen_vehicle x `piq ?opts)
let prerr_vehicle ?opts x =
  Pervasives.prerr_endline (gen_vehicle x `piq ?opts)

let print_player ?opts x =
  Pervasives.print_endline (gen_player x `piq ?opts)
let prerr_player ?opts x =
  Pervasives.prerr_endline (gen_player x `piq ?opts)

let print_graph ?opts x =
  Pervasives.print_endline (gen_graph x `piq ?opts)
let prerr_graph ?opts x =
  Pervasives.prerr_endline (gen_graph x `piq ?opts)

let print_vehiclelist ?opts x =
  Pervasives.print_endline (gen_vehiclelist x `piq ?opts)
let prerr_vehiclelist ?opts x =
  Pervasives.prerr_endline (gen_vehiclelist x `piq ?opts)

let print_playerlist ?opts x =
  Pervasives.print_endline (gen_playerlist x `piq ?opts)
let prerr_playerlist ?opts x =
  Pervasives.prerr_endline (gen_playerlist x `piq ?opts)

let print_gamestate ?opts x =
  Pervasives.print_endline (gen_gamestate x `piq ?opts)
let prerr_gamestate ?opts x =
  Pervasives.prerr_endline (gen_gamestate x `piq ?opts)


