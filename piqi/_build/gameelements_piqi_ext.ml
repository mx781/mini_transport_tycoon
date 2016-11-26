let piqi = Gameelements_piqi.piqi


let _ = Piqirun_ext.init_piqi piqi


let _rtype_piqi_type = Piqirun_ext.find_piqi_type "gameelements/rtype"
let _vtype_piqi_type = Piqirun_ext.find_piqi_type "gameelements/vtype"
let _vstatus_piqi_type = Piqirun_ext.find_piqi_type "gameelements/vstatus"
let _intopt_piqi_type = Piqirun_ext.find_piqi_type "gameelements/intopt"
let _good_piqi_type = Piqirun_ext.find_piqi_type "gameelements/good"
let _goodsprofile_piqi_type = Piqirun_ext.find_piqi_type "gameelements/goodsprofile"
let _gplist_piqi_type = Piqirun_ext.find_piqi_type "gameelements/gplist"
let _location_piqi_type = Piqirun_ext.find_piqi_type "gameelements/location"
let _destlist_piqi_type = Piqirun_ext.find_piqi_type "gameelements/destlist"
let _connection_piqi_type = Piqirun_ext.find_piqi_type "gameelements/connection"
let _vehicle_piqi_type = Piqirun_ext.find_piqi_type "gameelements/vehicle"


let parse_rtype ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _rtype_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Gameelements_piqi.parse_rtype buf

let parse_vtype ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _vtype_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Gameelements_piqi.parse_vtype buf

let parse_vstatus ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _vstatus_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Gameelements_piqi.parse_vstatus buf

let parse_intopt ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _intopt_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Gameelements_piqi.parse_intopt buf

let parse_good ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _good_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Gameelements_piqi.parse_good buf

let parse_goodsprofile ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _goodsprofile_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Gameelements_piqi.parse_goodsprofile buf

let parse_gplist ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _gplist_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Gameelements_piqi.parse_gplist buf

let parse_location ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _location_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Gameelements_piqi.parse_location buf

let parse_destlist ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _destlist_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Gameelements_piqi.parse_destlist buf

let parse_connection ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _connection_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Gameelements_piqi.parse_connection buf

let parse_vehicle ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _vehicle_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Gameelements_piqi.parse_vehicle buf


let gen_rtype ?opts x (format :Piqirun_ext.output_format) =
  let buf = Gameelements_piqi.gen_rtype x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _rtype_piqi_type `pb format x_pb ?opts

let gen_vtype ?opts x (format :Piqirun_ext.output_format) =
  let buf = Gameelements_piqi.gen_vtype x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _vtype_piqi_type `pb format x_pb ?opts

let gen_vstatus ?opts x (format :Piqirun_ext.output_format) =
  let buf = Gameelements_piqi.gen_vstatus x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _vstatus_piqi_type `pb format x_pb ?opts

let gen_intopt ?opts x (format :Piqirun_ext.output_format) =
  let buf = Gameelements_piqi.gen_intopt x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _intopt_piqi_type `pb format x_pb ?opts

let gen_good ?opts x (format :Piqirun_ext.output_format) =
  let buf = Gameelements_piqi.gen_good x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _good_piqi_type `pb format x_pb ?opts

let gen_goodsprofile ?opts x (format :Piqirun_ext.output_format) =
  let buf = Gameelements_piqi.gen_goodsprofile x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _goodsprofile_piqi_type `pb format x_pb ?opts

let gen_gplist ?opts x (format :Piqirun_ext.output_format) =
  let buf = Gameelements_piqi.gen_gplist x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _gplist_piqi_type `pb format x_pb ?opts

let gen_location ?opts x (format :Piqirun_ext.output_format) =
  let buf = Gameelements_piqi.gen_location x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _location_piqi_type `pb format x_pb ?opts

let gen_destlist ?opts x (format :Piqirun_ext.output_format) =
  let buf = Gameelements_piqi.gen_destlist x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _destlist_piqi_type `pb format x_pb ?opts

let gen_connection ?opts x (format :Piqirun_ext.output_format) =
  let buf = Gameelements_piqi.gen_connection x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _connection_piqi_type `pb format x_pb ?opts

let gen_vehicle ?opts x (format :Piqirun_ext.output_format) =
  let buf = Gameelements_piqi.gen_vehicle x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _vehicle_piqi_type `pb format x_pb ?opts


let print_rtype ?opts x =
  Pervasives.print_endline (gen_rtype x `piq ?opts)
let prerr_rtype ?opts x =
  Pervasives.prerr_endline (gen_rtype x `piq ?opts)

let print_vtype ?opts x =
  Pervasives.print_endline (gen_vtype x `piq ?opts)
let prerr_vtype ?opts x =
  Pervasives.prerr_endline (gen_vtype x `piq ?opts)

let print_vstatus ?opts x =
  Pervasives.print_endline (gen_vstatus x `piq ?opts)
let prerr_vstatus ?opts x =
  Pervasives.prerr_endline (gen_vstatus x `piq ?opts)

let print_intopt ?opts x =
  Pervasives.print_endline (gen_intopt x `piq ?opts)
let prerr_intopt ?opts x =
  Pervasives.prerr_endline (gen_intopt x `piq ?opts)

let print_good ?opts x =
  Pervasives.print_endline (gen_good x `piq ?opts)
let prerr_good ?opts x =
  Pervasives.prerr_endline (gen_good x `piq ?opts)

let print_goodsprofile ?opts x =
  Pervasives.print_endline (gen_goodsprofile x `piq ?opts)
let prerr_goodsprofile ?opts x =
  Pervasives.prerr_endline (gen_goodsprofile x `piq ?opts)

let print_gplist ?opts x =
  Pervasives.print_endline (gen_gplist x `piq ?opts)
let prerr_gplist ?opts x =
  Pervasives.prerr_endline (gen_gplist x `piq ?opts)

let print_location ?opts x =
  Pervasives.print_endline (gen_location x `piq ?opts)
let prerr_location ?opts x =
  Pervasives.prerr_endline (gen_location x `piq ?opts)

let print_destlist ?opts x =
  Pervasives.print_endline (gen_destlist x `piq ?opts)
let prerr_destlist ?opts x =
  Pervasives.prerr_endline (gen_destlist x `piq ?opts)

let print_connection ?opts x =
  Pervasives.print_endline (gen_connection x `piq ?opts)
let prerr_connection ?opts x =
  Pervasives.prerr_endline (gen_connection x `piq ?opts)

let print_vehicle ?opts x =
  Pervasives.print_endline (gen_vehicle x `piq ?opts)
let prerr_vehicle ?opts x =
  Pervasives.prerr_endline (gen_vehicle x `piq ?opts)


