module rec Gameelements_piqi:
  sig
    type float64 = float
    type float = Gameelements_piqi.float64
    type rtype =
      [
        | `lumber
        | `iron
        | `oil
        | `electronics
        | `produce
      ]
    type vtype =
      [
        | `car
        | `truck
      ]
    type vstatus =
      [
        | `waiting
        | `driving
        | `broken
      ]
    type traveltime =
      [
        | `none
        | `some of int
      ]
    type good = Good.t
    type goodsprofile = Goodsprofile.t
    type gplist = Gameelements_piqi.goodsprofile list
    type location = Location.t
    type destlist = int list
    type connection = Connection.t
    type vehicle = Vehicle.t
  end = Gameelements_piqi
and Good:
  sig
    type t = {
      mutable t: Gameelements_piqi.rtype;
      mutable quant: int;
    }
  end = Good
and Goodsprofile:
  sig
    type t = {
      mutable resource: Gameelements_piqi.rtype;
      mutable stepstoinc: int;
      mutable current: int;
      mutable capacity: int;
      mutable price: int;
      mutable naturalprice: int;
    }
  end = Goodsprofile
and Location:
  sig
    type t = {
      mutable id: int;
      mutable locationx: int;
      mutable locationy: int;
      mutable accepts: Gameelements_piqi.gplist;
      mutable produces: Gameelements_piqi.gplist;
    }
  end = Location
and Connection:
  sig
    type t = {
      mutable owner: int;
      mutable lstart: int;
      mutable lend: int;
      mutable age: int;
      mutable speedi: int;
      mutable speedj: int;
      mutable length: int;
    }
  end = Connection
and Vehicle:
  sig
    type t = {
      mutable owner: int;
      mutable t: Gameelements_piqi.vtype;
      mutable speed: Gameelements_piqi.float;
      mutable capacity: int;
      mutable cargo: Gameelements_piqi.good;
      mutable age: int;
      mutable status: Gameelements_piqi.vstatus;
      mutable x: int;
      mutable y: int;
      mutable destination: Gameelements_piqi.destlist;
      mutable predtraveltime: Gameelements_piqi.traveltime;
    }
  end = Vehicle


let rec parse_float64 x = Piqirun.float_of_fixed64 x
and packed_parse_float64 x = Piqirun.float_of_packed_fixed64 x

and parse_int x = Piqirun.int_of_zigzag_varint x
and packed_parse_int x = Piqirun.int_of_packed_zigzag_varint x

and parse_float x = parse_float64 x
and packed_parse_float x = packed_parse_float64 x

and parse_rtype x =
  let code, x = Piqirun.parse_variant x in
  match code with
    | 1 when x = Piqirun.Varint 1 -> `lumber
    | 2 when x = Piqirun.Varint 1 -> `iron
    | 3 when x = Piqirun.Varint 1 -> `oil
    | 4 when x = Piqirun.Varint 1 -> `electronics
    | 5 when x = Piqirun.Varint 1 -> `produce
    | _ -> Piqirun.error_variant x code

and parse_vtype x =
  let code, x = Piqirun.parse_variant x in
  match code with
    | 1 when x = Piqirun.Varint 1 -> `car
    | 2 when x = Piqirun.Varint 1 -> `truck
    | _ -> Piqirun.error_variant x code

and parse_vstatus x =
  let code, x = Piqirun.parse_variant x in
  match code with
    | 1 when x = Piqirun.Varint 1 -> `waiting
    | 2 when x = Piqirun.Varint 1 -> `driving
    | 3 when x = Piqirun.Varint 1 -> `broken
    | _ -> Piqirun.error_variant x code

and parse_traveltime x =
  let code, x = Piqirun.parse_variant x in
  match code with
    | 1 when x = Piqirun.Varint 1 -> `none
    | 2 ->
        let res = parse_int x in
        `some res
    | _ -> Piqirun.error_variant x code

and parse_good x =
  let x = Piqirun.parse_record x in
  let _t, x = Piqirun.parse_required_field 1 parse_rtype x in
  let _quant, x = Piqirun.parse_required_field 2 parse_int x in
  Piqirun.check_unparsed_fields x;
  {
    Good.t = _t;
    Good.quant = _quant;
  }

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

and parse_gplist x =
  Piqirun.parse_list (parse_goodsprofile) x


and parse_location x =
  let x = Piqirun.parse_record x in
  let _id, x = Piqirun.parse_required_field 1 parse_int x in
  let _locationx, x = Piqirun.parse_required_field 2 parse_int x in
  let _locationy, x = Piqirun.parse_required_field 3 parse_int x in
  let _accepts, x = Piqirun.parse_required_field 4 parse_gplist x in
  let _produces, x = Piqirun.parse_required_field 5 parse_gplist x in
  Piqirun.check_unparsed_fields x;
  {
    Location.id = _id;
    Location.locationx = _locationx;
    Location.locationy = _locationy;
    Location.accepts = _accepts;
    Location.produces = _produces;
  }

and parse_destlist x =
  Piqirun.parse_list (parse_int) x


and parse_connection x =
  let x = Piqirun.parse_record x in
  let _owner, x = Piqirun.parse_required_field 1 parse_int x in
  let _lstart, x = Piqirun.parse_required_field 2 parse_int x in
  let _lend, x = Piqirun.parse_required_field 3 parse_int x in
  let _age, x = Piqirun.parse_required_field 4 parse_int x in
  let _speedi, x = Piqirun.parse_required_field 5 parse_int x in
  let _speedj, x = Piqirun.parse_required_field 6 parse_int x in
  let _length, x = Piqirun.parse_required_field 7 parse_int x in
  Piqirun.check_unparsed_fields x;
  {
    Connection.owner = _owner;
    Connection.lstart = _lstart;
    Connection.lend = _lend;
    Connection.age = _age;
    Connection.speedi = _speedi;
    Connection.speedj = _speedj;
    Connection.length = _length;
  }

and parse_vehicle x =
  let x = Piqirun.parse_record x in
  let _owner, x = Piqirun.parse_required_field 1 parse_int x in
  let _t, x = Piqirun.parse_required_field 2 parse_vtype x in
  let _speed, x = Piqirun.parse_required_field 3 parse_float x in
  let _capacity, x = Piqirun.parse_required_field 4 parse_int x in
  let _cargo, x = Piqirun.parse_required_field 5 parse_good x in
  let _age, x = Piqirun.parse_required_field 6 parse_int x in
  let _status, x = Piqirun.parse_required_field 7 parse_vstatus x in
  let _x, x = Piqirun.parse_required_field 8 parse_int x in
  let _y, x = Piqirun.parse_required_field 9 parse_int x in
  let _destination, x = Piqirun.parse_required_field 10 parse_destlist x in
  let _predtraveltime, x = Piqirun.parse_required_field 11 parse_traveltime x in
  Piqirun.check_unparsed_fields x;
  {
    Vehicle.owner = _owner;
    Vehicle.t = _t;
    Vehicle.speed = _speed;
    Vehicle.capacity = _capacity;
    Vehicle.cargo = _cargo;
    Vehicle.age = _age;
    Vehicle.status = _status;
    Vehicle.x = _x;
    Vehicle.y = _y;
    Vehicle.destination = _destination;
    Vehicle.predtraveltime = _predtraveltime;
  }


let rec gen__float64 code x = Piqirun.float_to_fixed64 code x
and packed_gen__float64 x = Piqirun.float_to_packed_fixed64 x

and gen__int code x = Piqirun.int_to_zigzag_varint code x
and packed_gen__int x = Piqirun.int_to_packed_zigzag_varint x

and gen__float code x = gen__float64 code x
and packed_gen__float x = packed_gen__float64 x

and gen__rtype code (x:Gameelements_piqi.rtype) =
  Piqirun.gen_record code [(match x with
    | `lumber -> Piqirun.gen_bool_field 1 true
    | `iron -> Piqirun.gen_bool_field 2 true
    | `oil -> Piqirun.gen_bool_field 3 true
    | `electronics -> Piqirun.gen_bool_field 4 true
    | `produce -> Piqirun.gen_bool_field 5 true
  )]

and gen__vtype code (x:Gameelements_piqi.vtype) =
  Piqirun.gen_record code [(match x with
    | `car -> Piqirun.gen_bool_field 1 true
    | `truck -> Piqirun.gen_bool_field 2 true
  )]

and gen__vstatus code (x:Gameelements_piqi.vstatus) =
  Piqirun.gen_record code [(match x with
    | `waiting -> Piqirun.gen_bool_field 1 true
    | `driving -> Piqirun.gen_bool_field 2 true
    | `broken -> Piqirun.gen_bool_field 3 true
  )]

and gen__traveltime code (x:Gameelements_piqi.traveltime) =
  Piqirun.gen_record code [(match x with
    | `none -> Piqirun.gen_bool_field 1 true
    | `some x -> gen__int 2 x
  )]

and gen__good code x =
  let _t = Piqirun.gen_required_field 1 gen__rtype x.Good.t in
  let _quant = Piqirun.gen_required_field 2 gen__int x.Good.quant in
  Piqirun.gen_record code (_t :: _quant :: [])

and gen__goodsprofile code x =
  let _resource = Piqirun.gen_required_field 1 gen__rtype x.Goodsprofile.resource in
  let _stepstoinc = Piqirun.gen_required_field 2 gen__int x.Goodsprofile.stepstoinc in
  let _current = Piqirun.gen_required_field 3 gen__int x.Goodsprofile.current in
  let _capacity = Piqirun.gen_required_field 4 gen__int x.Goodsprofile.capacity in
  let _price = Piqirun.gen_required_field 5 gen__int x.Goodsprofile.price in
  let _naturalprice = Piqirun.gen_required_field 6 gen__int x.Goodsprofile.naturalprice in
  Piqirun.gen_record code (_resource :: _stepstoinc :: _current :: _capacity :: _price :: _naturalprice :: [])

and gen__gplist code x = (Piqirun.gen_list (gen__goodsprofile)) code x

and gen__location code x =
  let _id = Piqirun.gen_required_field 1 gen__int x.Location.id in
  let _locationx = Piqirun.gen_required_field 2 gen__int x.Location.locationx in
  let _locationy = Piqirun.gen_required_field 3 gen__int x.Location.locationy in
  let _accepts = Piqirun.gen_required_field 4 gen__gplist x.Location.accepts in
  let _produces = Piqirun.gen_required_field 5 gen__gplist x.Location.produces in
  Piqirun.gen_record code (_id :: _locationx :: _locationy :: _accepts :: _produces :: [])

and gen__destlist code x = (Piqirun.gen_list (gen__int)) code x

and gen__connection code x =
  let _owner = Piqirun.gen_required_field 1 gen__int x.Connection.owner in
  let _lstart = Piqirun.gen_required_field 2 gen__int x.Connection.lstart in
  let _lend = Piqirun.gen_required_field 3 gen__int x.Connection.lend in
  let _age = Piqirun.gen_required_field 4 gen__int x.Connection.age in
  let _speedi = Piqirun.gen_required_field 5 gen__int x.Connection.speedi in
  let _speedj = Piqirun.gen_required_field 6 gen__int x.Connection.speedj in
  let _length = Piqirun.gen_required_field 7 gen__int x.Connection.length in
  Piqirun.gen_record code (_owner :: _lstart :: _lend :: _age :: _speedi :: _speedj :: _length :: [])

and gen__vehicle code x =
  let _owner = Piqirun.gen_required_field 1 gen__int x.Vehicle.owner in
  let _t = Piqirun.gen_required_field 2 gen__vtype x.Vehicle.t in
  let _speed = Piqirun.gen_required_field 3 gen__float x.Vehicle.speed in
  let _capacity = Piqirun.gen_required_field 4 gen__int x.Vehicle.capacity in
  let _cargo = Piqirun.gen_required_field 5 gen__good x.Vehicle.cargo in
  let _age = Piqirun.gen_required_field 6 gen__int x.Vehicle.age in
  let _status = Piqirun.gen_required_field 7 gen__vstatus x.Vehicle.status in
  let _x = Piqirun.gen_required_field 8 gen__int x.Vehicle.x in
  let _y = Piqirun.gen_required_field 9 gen__int x.Vehicle.y in
  let _destination = Piqirun.gen_required_field 10 gen__destlist x.Vehicle.destination in
  let _predtraveltime = Piqirun.gen_required_field 11 gen__traveltime x.Vehicle.predtraveltime in
  Piqirun.gen_record code (_owner :: _t :: _speed :: _capacity :: _cargo :: _age :: _status :: _x :: _y :: _destination :: _predtraveltime :: [])


let gen_float64 x = gen__float64 (-1) x
let gen_int x = gen__int (-1) x
let gen_float x = gen__float (-1) x
let gen_rtype x = gen__rtype (-1) x
let gen_vtype x = gen__vtype (-1) x
let gen_vstatus x = gen__vstatus (-1) x
let gen_traveltime x = gen__traveltime (-1) x
let gen_good x = gen__good (-1) x
let gen_goodsprofile x = gen__goodsprofile (-1) x
let gen_gplist x = gen__gplist (-1) x
let gen_location x = gen__location (-1) x
let gen_destlist x = gen__destlist (-1) x
let gen_connection x = gen__connection (-1) x
let gen_vehicle x = gen__vehicle (-1) x


let rec default_float64 () = 0.0
and default_int () = 0
and default_float () = default_float64 ()
and default_rtype () = `lumber
and default_vtype () = `car
and default_vstatus () = `waiting
and default_traveltime () = `none
and default_good () =
  {
    Good.t = default_rtype ();
    Good.quant = default_int ();
  }
and default_goodsprofile () =
  {
    Goodsprofile.resource = default_rtype ();
    Goodsprofile.stepstoinc = default_int ();
    Goodsprofile.current = default_int ();
    Goodsprofile.capacity = default_int ();
    Goodsprofile.price = default_int ();
    Goodsprofile.naturalprice = default_int ();
  }
and default_gplist () = []
and default_location () =
  {
    Location.id = default_int ();
    Location.locationx = default_int ();
    Location.locationy = default_int ();
    Location.accepts = default_gplist ();
    Location.produces = default_gplist ();
  }
and default_destlist () = []
and default_connection () =
  {
    Connection.owner = default_int ();
    Connection.lstart = default_int ();
    Connection.lend = default_int ();
    Connection.age = default_int ();
    Connection.speedi = default_int ();
    Connection.speedj = default_int ();
    Connection.length = default_int ();
  }
and default_vehicle () =
  {
    Vehicle.owner = default_int ();
    Vehicle.t = default_vtype ();
    Vehicle.speed = default_float ();
    Vehicle.capacity = default_int ();
    Vehicle.cargo = default_good ();
    Vehicle.age = default_int ();
    Vehicle.status = default_vstatus ();
    Vehicle.x = default_int ();
    Vehicle.y = default_int ();
    Vehicle.destination = default_destlist ();
    Vehicle.predtraveltime = default_traveltime ();
  }


let piqi = "\226\202\2304\012gameelements\226\231\249\238\001\017gameelements.piqi\242\189\246\234\004\012ocaml-module\218\244\134\182\012\133\001\170\136\200\184\014\127\218\164\238\191\004\005rtype\170\183\218\222\005\017\232\146\150q\002\218\164\238\191\004\006Lumber\170\183\218\222\005\015\232\146\150q\004\218\164\238\191\004\004Iron\170\183\218\222\005\014\232\146\150q\006\218\164\238\191\004\003Oil\170\183\218\222\005\022\232\146\150q\b\218\164\238\191\004\011Electronics\170\183\218\222\005\018\232\146\150q\n\218\164\238\191\004\007Produce\218\244\134\182\012;\170\136\200\184\0145\218\164\238\191\004\005vtype\170\183\218\222\005\014\232\146\150q\002\218\164\238\191\004\003Car\170\183\218\222\005\016\232\146\150q\004\218\164\238\191\004\005Truck\218\244\134\182\012Z\170\136\200\184\014T\218\164\238\191\004\007vstatus\170\183\218\222\005\018\232\146\150q\002\218\164\238\191\004\007Waiting\170\183\218\222\005\018\232\146\150q\004\218\164\238\191\004\007Driving\170\183\218\222\005\017\232\146\150q\006\218\164\238\191\004\006Broken\218\244\134\182\012I\170\136\200\184\014C\218\164\238\191\004\ntraveltime\170\183\218\222\005\015\232\146\150q\002\218\164\238\191\004\004None\170\183\218\222\005\024\232\146\150q\004\218\164\238\191\004\004Some\210\171\158\194\006\003int\218\244\134\182\012^\138\233\142\251\014X\210\203\242$!\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\001t\210\171\158\194\006\005rtype\210\203\242$#\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005quant\210\171\158\194\006\003int\218\164\238\191\004\004good\218\244\134\182\012\159\002\138\233\142\251\014\152\002\210\203\242$(\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\bresource\210\171\158\194\006\005rtype\210\203\242$(\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\nstepstoinc\210\171\158\194\006\003int\210\203\242$%\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\007current\210\171\158\194\006\003int\210\203\242$&\232\146\150q\b\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\bcapacity\210\171\158\194\006\003int\210\203\242$#\232\146\150q\n\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005price\210\171\158\194\006\003int\210\203\242$*\232\146\150q\012\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\012naturalprice\210\171\158\194\006\003int\218\164\238\191\004\012goodsprofile\218\244\134\182\012$\242\197\227\236\003\030\218\164\238\191\004\006gplist\210\171\158\194\006\012goodsprofile\218\244\134\182\012\237\001\138\233\142\251\014\230\001\210\203\242$ \232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\002id\210\171\158\194\006\003int\210\203\242$'\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\tlocationx\210\171\158\194\006\003int\210\203\242$'\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\tlocationy\210\171\158\194\006\003int\210\203\242$(\232\146\150q\b\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\007accepts\210\171\158\194\006\006gplist\210\203\242$)\232\146\150q\n\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\bproduces\210\171\158\194\006\006gplist\218\164\238\191\004\blocation\218\244\134\182\012\029\242\197\227\236\003\023\218\164\238\191\004\bdestlist\210\171\158\194\006\003int\218\244\134\182\012\176\002\138\233\142\251\014\169\002\210\203\242$#\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005owner\210\171\158\194\006\003int\210\203\242$$\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006lstart\210\171\158\194\006\003int\210\203\242$\"\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004lend\210\171\158\194\006\003int\210\203\242$!\232\146\150q\b\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\003age\210\171\158\194\006\003int\210\203\242$$\232\146\150q\n\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006speedi\210\171\158\194\006\003int\210\203\242$$\232\146\150q\012\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006speedj\210\171\158\194\006\003int\210\203\242$$\232\146\150q\014\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006length\210\171\158\194\006\003int\218\164\238\191\004\nconnection\218\244\134\182\012\230\003\138\233\142\251\014\223\003\210\203\242$#\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005owner\210\171\158\194\006\003int\210\203\242$!\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\001t\210\171\158\194\006\005vtype\210\203\242$%\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005speed\210\171\158\194\006\005float\210\203\242$&\232\146\150q\b\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\bcapacity\210\171\158\194\006\003int\210\203\242$$\232\146\150q\n\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005cargo\210\171\158\194\006\004good\210\203\242$!\232\146\150q\012\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\003age\210\171\158\194\006\003int\210\203\242$(\232\146\150q\014\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006status\210\171\158\194\006\007vstatus\210\203\242$\031\232\146\150q\016\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\001x\210\171\158\194\006\003int\210\203\242$\031\232\146\150q\018\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\001y\210\171\158\194\006\003int\210\203\242$.\232\146\150q\020\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\011destination\210\171\158\194\006\bdestlist\210\203\242$3\232\146\150q\022\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\014predtraveltime\210\171\158\194\006\ntraveltime\218\164\238\191\004\007vehicle"
include Gameelements_piqi
