type r_type =
    | Lumber
    | Iron
    | Oil
    | Electronics
    | Produce

type gprof = 
{
  resource: r_type;
  steps_to_inc: int;
  current: int;
  capacity: int;
  price: int;
  natural_price: int;
}

open Exp_piqi.Goodsprofile

let gprof_of_goodsprofile: Exp_piqi.goodsprofile -> gprof = 
  (fun x -> {
    resource = Lumber;
    steps_to_inc = 21;
    current = 123;
    capacity = 12;
    price = 222;
    natural_price = 0;
  })
let gprof_to_goodsprofile: gprof -> Exp_piqi.goodsprofile = failwith "kaka"

