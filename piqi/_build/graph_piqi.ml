module  Gameelements = Gameelements_piqi

module rec Graph_piqi:
  sig
    type metadata = Metadata.t
    type node = Node.t
    type nodelist = Gameelements.location list
    type edge = Edge.t
    type edgelist = Gameelements.connection list
    type graph = Graph.t
  end = Graph_piqi
and Metadata:
  sig
    type t = {
      mutable var: string;
    }
  end = Metadata
and Node:
  sig
    type t = {
      mutable id: int;
      mutable type_: string;
      mutable label: string;
      mutable metadata: Gameelements.location;
    }
  end = Node
and Edge:
  sig
    type t = {
      mutable source: int;
      mutable relation: string;
      mutable destination: int;
      mutable directed: bool;
      mutable label: string;
      mutable metadata: Gameelements.connection;
    }
  end = Edge
and Graph:
  sig
    type t = {
      mutable directed: bool;
      mutable type_: string;
      mutable label: string;
      mutable metadata: Graph_piqi.metadata;
      mutable nodes: Graph_piqi.nodelist;
      mutable edges: Graph_piqi.edgelist;
    }
  end = Graph


let rec parse_string x = Piqirun.string_of_block x

and parse_int x = Piqirun.int_of_zigzag_varint x
and packed_parse_int x = Piqirun.int_of_packed_zigzag_varint x

and parse_bool x = Piqirun.bool_of_varint x
and packed_parse_bool x = Piqirun.bool_of_packed_varint x

and parse_metadata x =
  let x = Piqirun.parse_record x in
  let _var, x = Piqirun.parse_required_field 1 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Metadata.var = _var;
  }

and parse_node x =
  let x = Piqirun.parse_record x in
  let _id, x = Piqirun.parse_required_field 1 parse_int x in
  let _type_, x = Piqirun.parse_required_field 2 parse_string x in
  let _label, x = Piqirun.parse_required_field 3 parse_string x in
  let _metadata, x = Piqirun.parse_required_field 4 Gameelements.parse_location x in
  Piqirun.check_unparsed_fields x;
  {
    Node.id = _id;
    Node.type_ = _type_;
    Node.label = _label;
    Node.metadata = _metadata;
  }

and parse_nodelist x =
  Piqirun.parse_list (Gameelements.parse_location) x


and parse_edge x =
  let x = Piqirun.parse_record x in
  let _source, x = Piqirun.parse_required_field 1 parse_int x in
  let _relation, x = Piqirun.parse_required_field 2 parse_string x in
  let _destination, x = Piqirun.parse_required_field 3 parse_int x in
  let _directed, x = Piqirun.parse_required_field 4 parse_bool x in
  let _label, x = Piqirun.parse_required_field 5 parse_string x in
  let _metadata, x = Piqirun.parse_required_field 6 Gameelements.parse_connection x in
  Piqirun.check_unparsed_fields x;
  {
    Edge.source = _source;
    Edge.relation = _relation;
    Edge.destination = _destination;
    Edge.directed = _directed;
    Edge.label = _label;
    Edge.metadata = _metadata;
  }

and parse_edgelist x =
  Piqirun.parse_list (Gameelements.parse_connection) x


and parse_graph x =
  let x = Piqirun.parse_record x in
  let _directed, x = Piqirun.parse_required_field 1 parse_bool x in
  let _type_, x = Piqirun.parse_required_field 2 parse_string x in
  let _label, x = Piqirun.parse_required_field 3 parse_string x in
  let _metadata, x = Piqirun.parse_required_field 4 parse_metadata x in
  let _nodes, x = Piqirun.parse_required_field 5 parse_nodelist x in
  let _edges, x = Piqirun.parse_required_field 6 parse_edgelist x in
  Piqirun.check_unparsed_fields x;
  {
    Graph.directed = _directed;
    Graph.type_ = _type_;
    Graph.label = _label;
    Graph.metadata = _metadata;
    Graph.nodes = _nodes;
    Graph.edges = _edges;
  }


let rec gen__string code x = Piqirun.string_to_block code x

and gen__int code x = Piqirun.int_to_zigzag_varint code x
and packed_gen__int x = Piqirun.int_to_packed_zigzag_varint x

and gen__bool code x = Piqirun.bool_to_varint code x
and packed_gen__bool x = Piqirun.bool_to_packed_varint x

and gen__metadata code x =
  let _var = Piqirun.gen_required_field 1 gen__string x.Metadata.var in
  Piqirun.gen_record code (_var :: [])

and gen__node code x =
  let _id = Piqirun.gen_required_field 1 gen__int x.Node.id in
  let _type_ = Piqirun.gen_required_field 2 gen__string x.Node.type_ in
  let _label = Piqirun.gen_required_field 3 gen__string x.Node.label in
  let _metadata = Piqirun.gen_required_field 4 Gameelements.gen__location x.Node.metadata in
  Piqirun.gen_record code (_id :: _type_ :: _label :: _metadata :: [])

and gen__nodelist code x = (Piqirun.gen_list (Gameelements.gen__location)) code x

and gen__edge code x =
  let _source = Piqirun.gen_required_field 1 gen__int x.Edge.source in
  let _relation = Piqirun.gen_required_field 2 gen__string x.Edge.relation in
  let _destination = Piqirun.gen_required_field 3 gen__int x.Edge.destination in
  let _directed = Piqirun.gen_required_field 4 gen__bool x.Edge.directed in
  let _label = Piqirun.gen_required_field 5 gen__string x.Edge.label in
  let _metadata = Piqirun.gen_required_field 6 Gameelements.gen__connection x.Edge.metadata in
  Piqirun.gen_record code (_source :: _relation :: _destination :: _directed :: _label :: _metadata :: [])

and gen__edgelist code x = (Piqirun.gen_list (Gameelements.gen__connection)) code x

and gen__graph code x =
  let _directed = Piqirun.gen_required_field 1 gen__bool x.Graph.directed in
  let _type_ = Piqirun.gen_required_field 2 gen__string x.Graph.type_ in
  let _label = Piqirun.gen_required_field 3 gen__string x.Graph.label in
  let _metadata = Piqirun.gen_required_field 4 gen__metadata x.Graph.metadata in
  let _nodes = Piqirun.gen_required_field 5 gen__nodelist x.Graph.nodes in
  let _edges = Piqirun.gen_required_field 6 gen__edgelist x.Graph.edges in
  Piqirun.gen_record code (_directed :: _type_ :: _label :: _metadata :: _nodes :: _edges :: [])


let gen_string x = gen__string (-1) x
let gen_int x = gen__int (-1) x
let gen_bool x = gen__bool (-1) x
let gen_metadata x = gen__metadata (-1) x
let gen_node x = gen__node (-1) x
let gen_nodelist x = gen__nodelist (-1) x
let gen_edge x = gen__edge (-1) x
let gen_edgelist x = gen__edgelist (-1) x
let gen_graph x = gen__graph (-1) x


let rec default_string () = ""
and default_int () = 0
and default_bool () = false
and default_metadata () =
  {
    Metadata.var = default_string ();
  }
and default_node () =
  {
    Node.id = default_int ();
    Node.type_ = default_string ();
    Node.label = default_string ();
    Node.metadata = Gameelements.default_location ();
  }
and default_nodelist () = []
and default_edge () =
  {
    Edge.source = default_int ();
    Edge.relation = default_string ();
    Edge.destination = default_int ();
    Edge.directed = default_bool ();
    Edge.label = default_string ();
    Edge.metadata = Gameelements.default_connection ();
  }
and default_edgelist () = []
and default_graph () =
  {
    Graph.directed = default_bool ();
    Graph.type_ = default_string ();
    Graph.label = default_string ();
    Graph.metadata = default_metadata ();
    Graph.nodes = default_nodelist ();
    Graph.edges = default_edgelist ();
  }


let piqi = "\226\202\2304\005graph\226\231\249\238\001\ngraph.piqi\170\150\212\160\004\017\226\202\2304\012gameelements\242\189\246\234\004\012ocaml-module\242\189\246\234\004\nocaml-name\218\244\134\182\012=\138\233\142\251\0147\210\203\242$$\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\003var\210\171\158\194\006\006string\218\164\238\191\004\bmetadata\218\244\134\182\012\200\001\138\233\142\251\014\193\001\210\203\242$ \232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\002id\210\171\158\194\006\003int\210\203\242$%\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004type\210\171\158\194\006\006string\210\203\242$&\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005label\210\171\158\194\006\006string\210\203\242$8\232\146\150q\b\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\bmetadata\210\171\158\194\006\021gameelements/location\218\164\238\191\004\004node\218\244\134\182\012/\242\197\227\236\003)\218\164\238\191\004\bnodelist\210\171\158\194\006\021gameelements/location\218\244\134\182\012\172\002\138\233\142\251\014\165\002\210\203\242$$\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006source\210\171\158\194\006\003int\210\203\242$)\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\brelation\210\171\158\194\006\006string\210\203\242$)\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\011destination\210\171\158\194\006\003int\210\203\242$'\232\146\150q\b\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\bdirected\210\171\158\194\006\004bool\210\203\242$&\232\146\150q\n\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005label\210\171\158\194\006\006string\210\203\242$:\232\146\150q\012\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\bmetadata\210\171\158\194\006\023gameelements/connection\218\164\238\191\004\004edge\218\244\134\182\0121\242\197\227\236\003+\218\164\238\191\004\bedgelist\210\171\158\194\006\023gameelements/connection\218\244\134\182\012\157\002\138\233\142\251\014\150\002\210\203\242$'\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\bdirected\210\171\158\194\006\004bool\210\203\242$%\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004type\210\171\158\194\006\006string\210\203\242$&\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005label\210\171\158\194\006\006string\210\203\242$+\232\146\150q\b\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\bmetadata\210\171\158\194\006\bmetadata\210\203\242$(\232\146\150q\n\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005nodes\210\171\158\194\006\bnodelist\210\203\242$(\232\146\150q\012\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005edges\210\171\158\194\006\bedgelist\218\164\238\191\004\005graph"
include Graph_piqi
