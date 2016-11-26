let piqi = Player_piqi.piqi


let _ = Piqirun_ext.init_piqi piqi


let _ptype_piqi_type = Piqirun_ext.find_piqi_type "player/ptype"
let _player_piqi_type = Piqirun_ext.find_piqi_type "player/player"


let parse_ptype ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _ptype_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Player_piqi.parse_ptype buf

let parse_player ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _player_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Player_piqi.parse_player buf


let gen_ptype ?opts x (format :Piqirun_ext.output_format) =
  let buf = Player_piqi.gen_ptype x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _ptype_piqi_type `pb format x_pb ?opts

let gen_player ?opts x (format :Piqirun_ext.output_format) =
  let buf = Player_piqi.gen_player x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _player_piqi_type `pb format x_pb ?opts


let print_ptype ?opts x =
  Pervasives.print_endline (gen_ptype x `piq ?opts)
let prerr_ptype ?opts x =
  Pervasives.prerr_endline (gen_ptype x `piq ?opts)

let print_player ?opts x =
  Pervasives.print_endline (gen_player x `piq ?opts)
let prerr_player ?opts x =
  Pervasives.prerr_endline (gen_player x `piq ?opts)


