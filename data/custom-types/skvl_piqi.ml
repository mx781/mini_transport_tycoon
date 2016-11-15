module rec Skvl_piqi:
  sig
    type string_key_value = String_key_value.t
    type string_key_value_list = Skvl_piqi.string_key_value list
  end = Skvl_piqi
and String_key_value:
  sig
    type t = {
      mutable key: string;
      mutable value: string;
    }
  end = String_key_value


let rec parse_string x = Piqirun.string_of_block x

and parse_string_key_value x =
  let x = Piqirun.parse_record x in
  let _key, x = Piqirun.parse_required_field 1 parse_string x in
  let _value, x = Piqirun.parse_required_field 2 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    String_key_value.key = _key;
    String_key_value.value = _value;
  }

and parse_string_key_value_list x =
  Piqirun.parse_list (parse_string_key_value) x



let rec gen__string code x = Piqirun.string_to_block code x

and gen__string_key_value code x =
  let _key = Piqirun.gen_required_field 1 gen__string x.String_key_value.key in
  let _value = Piqirun.gen_required_field 2 gen__string x.String_key_value.value in
  Piqirun.gen_record code (_key :: _value :: [])

and gen__string_key_value_list code x = (Piqirun.gen_list (gen__string_key_value)) code x


let gen_string x = gen__string (-1) x
let gen_string_key_value x = gen__string_key_value (-1) x
let gen_string_key_value_list x = gen__string_key_value_list (-1) x


let rec default_string () = ""
and default_string_key_value () =
  {
    String_key_value.key = default_string ();
    String_key_value.value = default_string ();
  }
and default_string_key_value_list () = []


include Skvl_piqi
