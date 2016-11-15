type r_type =
    | Lumber
    | Iron
    | Oil
    | Electronics
    | Produce

type goods_profile = {
  resource: r_type;
  steps_to_inc: int;
  current: int;
  capacity: int;
  price: int;
  natural_price: int;
}