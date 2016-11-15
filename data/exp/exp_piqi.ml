module rec Exp_piqi:
  sig
    type rtype =
      [
        | `lumber
        | `iron
        | `oil
        | `electronics
        | `produce
      ]
    type goodsprofile = Goodsprofile.t
  end = Exp_piqi
and Goodsprofile:
  sig
    type t = {
      mutable resource: Exp_piqi.rtype;
      mutable stepstoinc: int;
      mutable current: int;
      mutable capacity: int;
      mutable price: int;
      mutable naturalprice: int;
    }
  end = Goodsprofile


let rec parse_int x = Piqirun.int_of_zigzag_varint x
and packed_parse_int x = Piqirun.int_of_packed_zigzag_varint x

and parse_rtype x =
  let code, x = Piqirun.parse_variant x in
  match code with
    | 1 when x = Piqirun.Varint 1 -> `lumber
    | 2 when x = Piqirun.Varint 1 -> `iron
    | 3 when x = Piqirun.Varint 1 -> `oil
    | 4 when x = Piqirun.Varint 1 -> `electronics
    | 5 when x = Piqirun.Varint 1 -> `produce
    | _ -> Piqirun.error_variant x code

and parse_goodsprofile x =
  let x = Piqirun.parse_record x in
  let _resource, x = Piqirun.parse_required_field 1 parse_rtype x in
  let _stepstoinc, x = Piqirun.parse_required_field 2 parse_int x in
  let _current, x = Piqirun.parse_required_field 3 parse_int x in
  let _capacity, x = Piqirun.parse_required_field 4 parse_int x in
  let _price, x = Piqirun.parse_required_field 5 parse_int x in
  let _naturalprice, x = Piqirun.parse_required_field 6 parse_int x in
  Piqirun.check_unparsed_fields x;
  {
    Goodsprofile.resource = _resource;
    Goodsprofile.stepstoinc = _stepstoinc;
    Goodsprofile.current = _current;
    Goodsprofile.capacity = _capacity;
    Goodsprofile.price = _price;
    Goodsprofile.naturalprice = _naturalprice;
  }


let rec gen__int code x = Piqirun.int_to_zigzag_varint code x
and packed_gen__int x = Piqirun.int_to_packed_zigzag_varint x

and gen__rtype code (x:Exp_piqi.rtype) =
  Piqirun.gen_record code [(match x with
    | `lumber -> Piqirun.gen_bool_field 1 true
    | `iron -> Piqirun.gen_bool_field 2 true
    | `oil -> Piqirun.gen_bool_field 3 true
    | `electronics -> Piqirun.gen_bool_field 4 true
    | `produce -> Piqirun.gen_bool_field 5 true
  )]

and gen__goodsprofile code x =
  let _resource = Piqirun.gen_required_field 1 gen__rtype x.Goodsprofile.resource in
  let _stepstoinc = Piqirun.gen_required_field 2 gen__int x.Goodsprofile.stepstoinc in
  let _current = Piqirun.gen_required_field 3 gen__int x.Goodsprofile.current in
  let _capacity = Piqirun.gen_required_field 4 gen__int x.Goodsprofile.capacity in
  let _price = Piqirun.gen_required_field 5 gen__int x.Goodsprofile.price in
  let _naturalprice = Piqirun.gen_required_field 6 gen__int x.Goodsprofile.naturalprice in
  Piqirun.gen_record code (_resource :: _stepstoinc :: _current :: _capacity :: _price :: _naturalprice :: [])


let gen_int x = gen__int (-1) x
let gen_rtype x = gen__rtype (-1) x
let gen_goodsprofile x = gen__goodsprofile (-1) x


let rec default_int () = 0
and default_rtype () = `lumber
and default_goodsprofile () =
  {
    Goodsprofile.resource = default_rtype ();
    Goodsprofile.stepstoinc = default_int ();
    Goodsprofile.current = default_int ();
    Goodsprofile.capacity = default_int ();
    Goodsprofile.price = default_int ();
    Goodsprofile.naturalprice = default_int ();
  }


include Exp_piqi
