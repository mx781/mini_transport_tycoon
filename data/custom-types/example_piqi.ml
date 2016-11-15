module  Skvl = Skvl_piqi

module rec Example_piqi:
  sig
    type char = Piqirun_custom.char
    type ocaml_nativeint = Piqirun_custom.nativeint
    type ocaml_bigint = Piqirun_custom.bigint
    type ocaml_string_key_value_list = Piqirun_custom.skvl
    type r = R.t
  end = Example_piqi
and R:
  sig
    type t = {
      mutable c: Example_piqi.char;
      mutable ni: Example_piqi.ocaml_nativeint;
      mutable bi: Example_piqi.ocaml_bigint;
      mutable kvl: Example_piqi.ocaml_string_key_value_list;
    }
  end = R


let rec parse_int x = Piqirun.int_of_zigzag_varint x
and packed_parse_int x = Piqirun.int_of_packed_zigzag_varint x

and parse_string x = Piqirun.string_of_block x

and parse_char x = (Piqirun_custom.char_of_int(parse_int x))
and packed_parse_char x = (Piqirun_custom.char_of_int(packed_parse_int x))

and parse_ocaml_nativeint x = (Piqirun_custom.nativeint_of_int(parse_int x))
and packed_parse_ocaml_nativeint x = (Piqirun_custom.nativeint_of_int(packed_parse_int x))

and parse_ocaml_bigint x = (Piqirun_custom.bigint_of_string(parse_string x))

and parse_ocaml_string_key_value_list x = (Piqirun_custom.skvl_of_string_key_value_list(Skvl.parse_string_key_value_list x))

and parse_r x =
  let x = Piqirun.parse_record x in
  let _c, x = Piqirun.parse_required_field 1 parse_char x in
  let _ni, x = Piqirun.parse_required_field 2 parse_ocaml_nativeint x in
  let _bi, x = Piqirun.parse_required_field 3 parse_ocaml_bigint x in
  let _kvl, x = Piqirun.parse_required_field 4 parse_ocaml_string_key_value_list x in
  Piqirun.check_unparsed_fields x;
  {
    R.c = _c;
    R.ni = _ni;
    R.bi = _bi;
    R.kvl = _kvl;
  }


let rec gen__int code x = Piqirun.int_to_zigzag_varint code x
and packed_gen__int x = Piqirun.int_to_packed_zigzag_varint x

and gen__string code x = Piqirun.string_to_block code x

and gen__char code x = gen__int code(Piqirun_custom.char_to_int( x))
and packed_gen__char x = packed_gen__int(Piqirun_custom.char_to_int( x))

and gen__ocaml_nativeint code x = gen__int code(Piqirun_custom.nativeint_to_int( x))
and packed_gen__ocaml_nativeint x = packed_gen__int(Piqirun_custom.nativeint_to_int( x))

and gen__ocaml_bigint code x = gen__string code(Piqirun_custom.bigint_to_string( x))

and gen__ocaml_string_key_value_list code x = Skvl.gen__string_key_value_list code(Piqirun_custom.skvl_to_string_key_value_list( x))

and gen__r code x =
  let _c = Piqirun.gen_required_field 1 gen__char x.R.c in
  let _ni = Piqirun.gen_required_field 2 gen__ocaml_nativeint x.R.ni in
  let _bi = Piqirun.gen_required_field 3 gen__ocaml_bigint x.R.bi in
  let _kvl = Piqirun.gen_required_field 4 gen__ocaml_string_key_value_list x.R.kvl in
  Piqirun.gen_record code (_c :: _ni :: _bi :: _kvl :: [])


let gen_int x = gen__int (-1) x
let gen_string x = gen__string (-1) x
let gen_char x = gen__char (-1) x
let gen_ocaml_nativeint x = gen__ocaml_nativeint (-1) x
let gen_ocaml_bigint x = gen__ocaml_bigint (-1) x
let gen_ocaml_string_key_value_list x = gen__ocaml_string_key_value_list (-1) x
let gen_r x = gen__r (-1) x


let rec default_int () = 0
and default_string () = ""
and default_char () = (Piqirun_custom.char_of_int(default_int ()))
and default_ocaml_nativeint () = (Piqirun_custom.nativeint_of_int(default_int ()))
and default_ocaml_bigint () = (Piqirun_custom.bigint_of_string(default_string ()))
and default_ocaml_string_key_value_list () = (Piqirun_custom.skvl_of_string_key_value_list(Skvl.default_string_key_value_list ()))
and default_r () =
  {
    R.c = default_char ();
    R.ni = default_ocaml_nativeint ();
    R.bi = default_ocaml_bigint ();
    R.kvl = default_ocaml_string_key_value_list ();
  }


include Example_piqi
