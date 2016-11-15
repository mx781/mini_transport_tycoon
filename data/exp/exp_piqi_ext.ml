let piqi = Exp_piqi.piqi


let _ = Piqirun_ext.init_piqi piqi


let _rtype_piqi_type = Piqirun_ext.find_piqi_type "exp/rtype"
let _goodsprofile_piqi_type = Piqirun_ext.find_piqi_type "exp/goodsprofile"


let parse_rtype ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _rtype_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Exp_piqi.parse_rtype buf

let parse_goodsprofile ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _goodsprofile_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Exp_piqi.parse_goodsprofile buf


let gen_rtype ?opts x (format :Piqirun_ext.output_format) =
  let buf = Exp_piqi.gen_rtype x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _rtype_piqi_type `pb format x_pb ?opts

let gen_goodsprofile ?opts x (format :Piqirun_ext.output_format) =
  let buf = Exp_piqi.gen_goodsprofile x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _goodsprofile_piqi_type `pb format x_pb ?opts


let print_rtype ?opts x =
  Pervasives.print_endline (gen_rtype x `piq ?opts)
let prerr_rtype ?opts x =
  Pervasives.prerr_endline (gen_rtype x `piq ?opts)

let print_goodsprofile ?opts x =
  Pervasives.print_endline (gen_goodsprofile x `piq ?opts)
let prerr_goodsprofile ?opts x =
  Pervasives.prerr_endline (gen_goodsprofile x `piq ?opts)


