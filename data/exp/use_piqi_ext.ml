module Exp_ext = Exp_piqi_ext
let piqi = Use_piqi.piqi


let _ = Piqirun_ext.init_piqi piqi


let _gprof_piqi_type = Piqirun_ext.find_piqi_type "use/gprof"
let _gamestate_piqi_type = Piqirun_ext.find_piqi_type "use/gamestate"


let parse_gprof ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _gprof_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Use_piqi.parse_gprof buf

let parse_gamestate ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _gamestate_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Use_piqi.parse_gamestate buf


let gen_gprof ?opts x (format :Piqirun_ext.output_format) =
  let buf = Use_piqi.gen_gprof x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _gprof_piqi_type `pb format x_pb ?opts

let gen_gamestate ?opts x (format :Piqirun_ext.output_format) =
  let buf = Use_piqi.gen_gamestate x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _gamestate_piqi_type `pb format x_pb ?opts


let print_gprof ?opts x =
  Pervasives.print_endline (gen_gprof x `piq ?opts)
let prerr_gprof ?opts x =
  Pervasives.prerr_endline (gen_gprof x `piq ?opts)

let print_gamestate ?opts x =
  Pervasives.print_endline (gen_gamestate x `piq ?opts)
let prerr_gamestate ?opts x =
  Pervasives.prerr_endline (gen_gamestate x `piq ?opts)


