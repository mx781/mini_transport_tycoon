module Gameelements_ext = Gameelements_piqi_ext
let piqi = Graph_piqi.piqi


let _ = Piqirun_ext.init_piqi piqi


let _metadata_piqi_type = Piqirun_ext.find_piqi_type "graph/metadata"
let _node_piqi_type = Piqirun_ext.find_piqi_type "graph/node"
let _nodelist_piqi_type = Piqirun_ext.find_piqi_type "graph/nodelist"
let _edge_piqi_type = Piqirun_ext.find_piqi_type "graph/edge"
let _edgelist_piqi_type = Piqirun_ext.find_piqi_type "graph/edgelist"
let _graph_piqi_type = Piqirun_ext.find_piqi_type "graph/graph"


let parse_metadata ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _metadata_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Graph_piqi.parse_metadata buf

let parse_node ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _node_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Graph_piqi.parse_node buf

let parse_nodelist ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _nodelist_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Graph_piqi.parse_nodelist buf

let parse_edge ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _edge_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Graph_piqi.parse_edge buf

let parse_edgelist ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _edgelist_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Graph_piqi.parse_edgelist buf

let parse_graph ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _graph_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Graph_piqi.parse_graph buf


let gen_metadata ?opts x (format :Piqirun_ext.output_format) =
  let buf = Graph_piqi.gen_metadata x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _metadata_piqi_type `pb format x_pb ?opts

let gen_node ?opts x (format :Piqirun_ext.output_format) =
  let buf = Graph_piqi.gen_node x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _node_piqi_type `pb format x_pb ?opts

let gen_nodelist ?opts x (format :Piqirun_ext.output_format) =
  let buf = Graph_piqi.gen_nodelist x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _nodelist_piqi_type `pb format x_pb ?opts

let gen_edge ?opts x (format :Piqirun_ext.output_format) =
  let buf = Graph_piqi.gen_edge x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _edge_piqi_type `pb format x_pb ?opts

let gen_edgelist ?opts x (format :Piqirun_ext.output_format) =
  let buf = Graph_piqi.gen_edgelist x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _edgelist_piqi_type `pb format x_pb ?opts

let gen_graph ?opts x (format :Piqirun_ext.output_format) =
  let buf = Graph_piqi.gen_graph x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _graph_piqi_type `pb format x_pb ?opts


let print_metadata ?opts x =
  Pervasives.print_endline (gen_metadata x `piq ?opts)
let prerr_metadata ?opts x =
  Pervasives.prerr_endline (gen_metadata x `piq ?opts)

let print_node ?opts x =
  Pervasives.print_endline (gen_node x `piq ?opts)
let prerr_node ?opts x =
  Pervasives.prerr_endline (gen_node x `piq ?opts)

let print_nodelist ?opts x =
  Pervasives.print_endline (gen_nodelist x `piq ?opts)
let prerr_nodelist ?opts x =
  Pervasives.prerr_endline (gen_nodelist x `piq ?opts)

let print_edge ?opts x =
  Pervasives.print_endline (gen_edge x `piq ?opts)
let prerr_edge ?opts x =
  Pervasives.prerr_endline (gen_edge x `piq ?opts)

let print_edgelist ?opts x =
  Pervasives.print_endline (gen_edgelist x `piq ?opts)
let prerr_edgelist ?opts x =
  Pervasives.prerr_endline (gen_edgelist x `piq ?opts)

let print_graph ?opts x =
  Pervasives.print_endline (gen_graph x `piq ?opts)
let prerr_graph ?opts x =
  Pervasives.prerr_endline (gen_graph x `piq ?opts)


