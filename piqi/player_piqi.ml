module rec Player_piqi:
  sig
    type float64 = float
    type float = Player_piqi.float64
    type ptype =
      [
        | `human
        | `ai of int
      ]
    type player = Player.t
  end = Player_piqi
and Player:
  sig
    type t = {
      mutable pid: int;
      mutable ptype: Player_piqi.ptype;
      mutable money: Player_piqi.float;
    }
  end = Player


let rec parse_float64 x = Piqirun.float_of_fixed64 x
and packed_parse_float64 x = Piqirun.float_of_packed_fixed64 x

and parse_int x = Piqirun.int_of_zigzag_varint x
and packed_parse_int x = Piqirun.int_of_packed_zigzag_varint x

and parse_float x = parse_float64 x
and packed_parse_float x = packed_parse_float64 x

and parse_ptype x =
  let code, x = Piqirun.parse_variant x in
  match code with
    | 1 when x = Piqirun.Varint 1 -> `human
    | 2 ->
        let res = parse_int x in
        `ai res
    | _ -> Piqirun.error_variant x code

and parse_player x =
  let x = Piqirun.parse_record x in
  let _pid, x = Piqirun.parse_required_field 1 parse_int x in
  let _ptype, x = Piqirun.parse_required_field 2 parse_ptype x in
  let _money, x = Piqirun.parse_required_field 3 parse_float x in
  Piqirun.check_unparsed_fields x;
  {
    Player.pid = _pid;
    Player.ptype = _ptype;
    Player.money = _money;
  }


let rec gen__float64 code x = Piqirun.float_to_fixed64 code x
and packed_gen__float64 x = Piqirun.float_to_packed_fixed64 x

and gen__int code x = Piqirun.int_to_zigzag_varint code x
and packed_gen__int x = Piqirun.int_to_packed_zigzag_varint x

and gen__float code x = gen__float64 code x
and packed_gen__float x = packed_gen__float64 x

and gen__ptype code (x:Player_piqi.ptype) =
  Piqirun.gen_record code [(match x with
    | `human -> Piqirun.gen_bool_field 1 true
    | `ai x -> gen__int 2 x
  )]

and gen__player code x =
  let _pid = Piqirun.gen_required_field 1 gen__int x.Player.pid in
  let _ptype = Piqirun.gen_required_field 2 gen__ptype x.Player.ptype in
  let _money = Piqirun.gen_required_field 3 gen__float x.Player.money in
  Piqirun.gen_record code (_pid :: _ptype :: _money :: [])


let gen_float64 x = gen__float64 (-1) x
let gen_int x = gen__int (-1) x
let gen_float x = gen__float (-1) x
let gen_ptype x = gen__ptype (-1) x
let gen_player x = gen__player (-1) x


let rec default_float64 () = 0.0
and default_int () = 0
and default_float () = default_float64 ()
and default_ptype () = `human
and default_player () =
  {
    Player.pid = default_int ();
    Player.ptype = default_ptype ();
    Player.money = default_float ();
  }


let piqi = "\226\202\2304\006player\226\231\249\238\001\011player.piqi\242\189\246\234\004\012ocaml-module\218\244\134\182\012C\170\136\200\184\014=\218\164\238\191\004\005ptype\170\183\218\222\005\016\232\146\150q\002\218\164\238\191\004\005Human\170\183\218\222\005\022\232\146\150q\004\218\164\238\191\004\002AI\210\171\158\194\006\003int\218\244\134\182\012\141\001\138\233\142\251\014\134\001\210\203\242$!\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\003pid\210\171\158\194\006\003int\210\203\242$%\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005ptype\210\171\158\194\006\005ptype\210\203\242$%\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005money\210\171\158\194\006\005float\218\164\238\191\004\006player"
include Player_piqi
