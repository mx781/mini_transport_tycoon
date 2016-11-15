module  Exp = Exp_piqi

module rec Use_piqi:
  sig
    type gprof = Piqirun_custom.gprof
    type gamestate = Gamestate.t
  end = Use_piqi
and Gamestate:
  sig
    type t = {
      mutable profile: Use_piqi.gprof;
    }
  end = Gamestate


let rec parse_gprof x = (Piqirun_custom.gprof_of_goodsprofile(Exp.parse_goodsprofile x))

and parse_gamestate x =
  let x = Piqirun.parse_record x in
  let _profile, x = Piqirun.parse_required_field 1 parse_gprof x in
  Piqirun.check_unparsed_fields x;
  {
    Gamestate.profile = _profile;
  }


let rec gen__gprof code x = Exp.gen__goodsprofile code(Piqirun_custom.gprof_to_goodsprofile( x))

and gen__gamestate code x =
  let _profile = Piqirun.gen_required_field 1 gen__gprof x.Gamestate.profile in
  Piqirun.gen_record code (_profile :: [])


let gen_gprof x = gen__gprof (-1) x
let gen_gamestate x = gen__gamestate (-1) x


let rec default_gprof () = (Piqirun_custom.gprof_of_goodsprofile(Exp.default_goodsprofile ()))
and default_gamestate () =
  {
    Gamestate.profile = default_gprof ();
  }


include Use_piqi
