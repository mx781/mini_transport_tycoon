(* Piqi object types *)
open Gameelements_piqi.Vehicle
open Gameelements_piqi.Good
open Player_piqi.Player
open Graph_piqi.Graph

(* Native typedefs need to be repeated manually -- opening the whole module
* overrides the Piqi objects. *)
type vehicle = GameElements.vehicle
type player = Player.player

(* Conversion functions between Piqi objects and native OCaml objects *)
(* for Vehicle *)
let vehicle_of_vehicle: Gameelements_piqi.vehicle -> GameElements.vehicle =
  (fun pv -> 
      let open GameElements in match pv with
      | {owner=o; t=t; speed=sp; capacity = cap; cargo=cg; age=age'; status=st;
        x=x'; y=y'; destination=dst; predtraveltime=ptt} ->
        let t' = match t with 
          | `car -> Car
          | `truck -> Truck
        in
        let cg' = match cg with
          | {t=t; quant=q;} -> 
            let t' = match t with
              | `lumber -> Lumber
              | `iron -> Iron
              | `oil -> Oil
              | `electronics -> Electronics
              | `produce -> Produce
            in 
            {t=t'; quantity=q}
        in
        let st' = match st with 
          | `waiting -> Waiting
          | `driving -> Driving
          | `broken -> Broken
        in
        let ptt' = match ptt with
          | `some x -> Some x
          | `none -> None
        in
        {
          v_owner_id = o;
          t = t';
          speed = sp;
          capacity = cap;
          cargo = cg';
          age = age';
          status = st';
          x = x';
          y = y';
          destination = dst;
          predicted_travel_time = ptt';
        }
  )
let vehicle_to_vehicle: GameElements.vehicle -> Gameelements_piqi.vehicle = (fun x -> failwith "unimplemented")

(* for Player *)
let player_of_player: Player_piqi.player -> Player.player = (fun p -> 
      let open Player in match p with
      | {pid=id'; ptype=pt; money=m;} ->
        let pt' = match pt with
          | `human -> Human
          | `ai x -> AI x
        in
        {
          p_id = id';
          p_type = pt';
          money = m;
        }
  )
let player_to_player: Player.player -> Player_piqi.player = (fun x -> failwith "unimplemented")