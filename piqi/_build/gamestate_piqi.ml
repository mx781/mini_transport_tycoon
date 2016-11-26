module  Player = Player_piqi

module  Graph = Graph_piqi

module  Gameelements = Gameelements_piqi

module rec Gamestate_piqi:
  sig
    type vehicle = Piqirun_custom.vehicle
    type player = Piqirun_custom.player
    type graph = Piqirun_custom.graph
    type funcopt =
      [
        | `none
      ]
    type vehiclelist = Gamestate_piqi.vehicle list
    type playerlist = Gamestate_piqi.player list
    type gamestate = Gamestate.t
  end = Gamestate_piqi
and Gamestate:
  sig
    type t = {
      mutable vehicles: Gamestate_piqi.vehiclelist;
      mutable players: Gamestate_piqi.playerlist;
      mutable aiinfo: Gamestate_piqi.funcopt;
      mutable gameage: int;
      mutable paused: bool;
      mutable graph: Gamestate_piqi.graph;
    }
  end = Gamestate


let rec parse_int x = Piqirun.int_of_zigzag_varint x
and packed_parse_int x = Piqirun.int_of_packed_zigzag_varint x

and parse_bool x = Piqirun.bool_of_varint x
and packed_parse_bool x = Piqirun.bool_of_packed_varint x

and parse_vehicle x = (Piqirun_custom.vehicle_of_vehicle(Gameelements.parse_vehicle x))

and parse_player x = (Piqirun_custom.player_of_player(Player.parse_player x))

and parse_graph x = (Piqirun_custom.graph_of_graph(Graph.parse_graph x))

and parse_vehiclelist x =
  Piqirun.parse_list (parse_vehicle) x


and parse_playerlist x =
  Piqirun.parse_list (parse_player) x


and parse_funcopt x =
  let code, x = Piqirun.parse_variant x in
  match code with
    | 1 when x = Piqirun.Varint 1 -> `none
    | _ -> Piqirun.error_variant x code

and parse_gamestate x =
  let x = Piqirun.parse_record x in
  let _vehicles, x = Piqirun.parse_required_field 1 parse_vehiclelist x in
  let _players, x = Piqirun.parse_required_field 2 parse_playerlist x in
  let _aiinfo, x = Piqirun.parse_required_field 3 parse_funcopt x in
  let _gameage, x = Piqirun.parse_required_field 4 parse_int x in
  let _paused, x = Piqirun.parse_required_field 5 parse_bool x in
  let _graph, x = Piqirun.parse_required_field 6 parse_graph x in
  Piqirun.check_unparsed_fields x;
  {
    Gamestate.vehicles = _vehicles;
    Gamestate.players = _players;
    Gamestate.aiinfo = _aiinfo;
    Gamestate.gameage = _gameage;
    Gamestate.paused = _paused;
    Gamestate.graph = _graph;
  }


let rec gen__int code x = Piqirun.int_to_zigzag_varint code x
and packed_gen__int x = Piqirun.int_to_packed_zigzag_varint x

and gen__bool code x = Piqirun.bool_to_varint code x
and packed_gen__bool x = Piqirun.bool_to_packed_varint x

and gen__vehicle code x = Gameelements.gen__vehicle code(Piqirun_custom.vehicle_to_vehicle( x))

and gen__player code x = Player.gen__player code(Piqirun_custom.player_to_player( x))

and gen__graph code x = Graph.gen__graph code(Piqirun_custom.graph_to_graph( x))

and gen__vehiclelist code x = (Piqirun.gen_list (gen__vehicle)) code x

and gen__playerlist code x = (Piqirun.gen_list (gen__player)) code x

and gen__funcopt code (x:Gamestate_piqi.funcopt) =
  Piqirun.gen_record code [(match x with
    | `none -> Piqirun.gen_bool_field 1 true
  )]

and gen__gamestate code x =
  let _vehicles = Piqirun.gen_required_field 1 gen__vehiclelist x.Gamestate.vehicles in
  let _players = Piqirun.gen_required_field 2 gen__playerlist x.Gamestate.players in
  let _aiinfo = Piqirun.gen_required_field 3 gen__funcopt x.Gamestate.aiinfo in
  let _gameage = Piqirun.gen_required_field 4 gen__int x.Gamestate.gameage in
  let _paused = Piqirun.gen_required_field 5 gen__bool x.Gamestate.paused in
  let _graph = Piqirun.gen_required_field 6 gen__graph x.Gamestate.graph in
  Piqirun.gen_record code (_vehicles :: _players :: _aiinfo :: _gameage :: _paused :: _graph :: [])


let gen_int x = gen__int (-1) x
let gen_bool x = gen__bool (-1) x
let gen_vehicle x = gen__vehicle (-1) x
let gen_player x = gen__player (-1) x
let gen_graph x = gen__graph (-1) x
let gen_vehiclelist x = gen__vehiclelist (-1) x
let gen_playerlist x = gen__playerlist (-1) x
let gen_funcopt x = gen__funcopt (-1) x
let gen_gamestate x = gen__gamestate (-1) x


let rec default_int () = 0
and default_bool () = false
and default_vehicle () = (Piqirun_custom.vehicle_of_vehicle(Gameelements.default_vehicle ()))
and default_player () = (Piqirun_custom.player_of_player(Player.default_player ()))
and default_graph () = (Piqirun_custom.graph_of_graph(Graph.default_graph ()))
and default_vehiclelist () = []
and default_playerlist () = []
and default_funcopt () = `none
and default_gamestate () =
  {
    Gamestate.vehicles = default_vehiclelist ();
    Gamestate.players = default_playerlist ();
    Gamestate.aiinfo = default_funcopt ();
    Gamestate.gameage = default_int ();
    Gamestate.paused = default_bool ();
    Gamestate.graph = default_graph ();
  }


let piqi = "\226\202\2304\tgamestate\226\231\249\238\001\014gamestate.piqi\154\219\203\172\003R.record [\n    .name gsl\n    .field [\n        .name gs\n        .type gslist\n    ]\n]\170\150\212\160\004\011\226\202\2304\006player\170\150\212\160\004\n\226\202\2304\005graph\170\150\212\160\004\017\226\202\2304\012gameelements\242\189\246\234\004\nocaml-name\242\189\246\234\004\012ocaml-module\242\189\246\234\004\nocaml-type\218\244\134\182\012U\130\153\170dP\218\164\238\191\004\007vehicle\210\171\158\194\006\020gameelements/vehicle\226\128\157\190\n\007vehicle\218\135\205\192\012\022Piqirun_custom.vehicle\218\244\134\182\012K\130\153\170dF\218\164\238\191\004\006player\210\171\158\194\006\rplayer/player\226\128\157\190\n\006player\218\135\205\192\012\021Piqirun_custom.player\218\244\134\182\012F\130\153\170dA\218\164\238\191\004\005graph\210\171\158\194\006\011graph/graph\226\128\157\190\n\005graph\218\135\205\192\012\020Piqirun_custom.graph\218\244\134\182\012$\242\197\227\236\003\030\218\164\238\191\004\011vehiclelist\210\171\158\194\006\007vehicle\218\244\134\182\012\"\242\197\227\236\003\028\218\164\238\191\004\nplayerlist\210\171\158\194\006\006player\218\244\134\182\012(\170\136\200\184\014\"\218\164\238\191\004\007funcopt\170\183\218\222\005\015\232\146\150q\002\218\164\238\191\004\004None\218\244\134\182\012\165\002\138\233\142\251\014\158\002\210\203\242$.\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\bvehicles\210\171\158\194\006\011vehiclelist\210\203\242$,\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\007players\210\171\158\194\006\nplayerlist\210\203\242$(\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006aiinfo\210\171\158\194\006\007funcopt\210\203\242$%\232\146\150q\b\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\007gameage\210\171\158\194\006\003int\210\203\242$%\232\146\150q\n\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006paused\210\171\158\194\006\004bool\210\203\242$%\232\146\150q\012\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005graph\210\171\158\194\006\005graph\218\164\238\191\004\tgamestate"
include Gamestate_piqi
