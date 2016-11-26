(* Piqi object types *)
open Gameelements_piqi.Vehicle
open Gameelements_piqi.Good
open Player_piqi.Player
open Graph_piqi.Graph
(* open Gamestate_piqi *)

exception ParsingFailure

(* Native typedefs need to be repeated manually -- opening the whole module
* overrides the Piqi objects. *)
type vehicle = GameElements.vehicle
type player = Player.player
type graph = GameElements.Map.t
type game_state = GameElements.game_state

(* Conversion functions between Piqi objects and native OCaml objects *)
(* for Vehicle *)
(* TODO: split out into smaller, reusable functions (good, vehstatus, vehtype)*)

let resource_of_piqiresource res = 
  let open GameElements in match res with
  | `lumber -> Lumber
  | `iron -> Iron
  | `oil -> Oil
  | `electronics -> Electronics
  | `produce -> Produce

let vehicle_of_vehicle: Gameelements_piqi.vehicle -> GameElements.vehicle =
  (fun pv -> 
    let open GameElements in match pv with
    | {owner=o; t=t; speed=sp; capacity = cap; cargo=cg; age=age'; status=st;
      x=x'; y=y'; destination=dst} ->
      let t' = match t with 
        | `car -> Car
        | `truck -> Truck
      in
      let cg' = match cg with
        | {t=t; quant=q;} -> 
          let t' = resource_of_piqiresource t in 
          {t=t'; quantity=q}
      in
      let st' = match st with 
        | `waiting -> Waiting
        | `driving -> Driving
        | `broken -> Broken
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

(* for Graph *)

let goodsprofile_of_piqigoodsprofile: Gameelements_piqi.Goodsprofile.t -> GameElements.goods_profile = 
(fun pgoodsprofile -> 
  let open GameElements in match pgoodsprofile with 
  | {resource=r; stepstoinc=sti; current=curr; capacity=cap; price=p;
    naturalprice = np} ->
    {
      resource = resource_of_piqiresource r;
      steps_to_inc = sti;
      current = curr;
      capacity = cap;
      price = p;
      natural_price = np;
    }
)

let node_of_piqinode: Gameelements_piqi.Location.t -> GameElements.location = 
(fun pnode -> 
  let open GameElements in match pnode with
  | {id=id'; lx=lx'; ly=ly'; accepts=a; produces=p} ->
    let a' = List.map goodsprofile_of_piqigoodsprofile a in
    let p' = List.map goodsprofile_of_piqigoodsprofile p in
    {
      l_id = id';
      lx = lx';
      ly = ly';
      accepts = a';
      produces = p';
    }
)

let edge_of_piqiedge: Gameelements_piqi.Connection.t -> GameElements.connection = 
(fun pedge ->
  let open GameElements in match pedge with
  | {owner=o; lstart=st; lend=end'; age=age'; speed=sp; length=l} ->
    {
      c_owner_id = o;
      l_start = st;
      l_end = end';
      age = age';
      speed = sp;
      length = l;
    }
)

let rec fetch_edge_by_id nodes id =
  let open GameElements in match nodes with
  | [] -> raise ParsingFailure
  | h::t -> if h.l_id = id then h else fetch_edge_by_id t id

let rec fetch_edge_nodes edges nodes edges_with_nodes = 
  let open GameElements in match edges with
  | [] -> edges_with_nodes
  | e::es -> 
    let s_id = e.l_start in
    let e_id = e.l_end in
    try
      let s_node = fetch_edge_by_id nodes s_id in
      let e_node = fetch_edge_by_id nodes e_id in
      fetch_edge_nodes es nodes ((s_node, e, e_node) :: edges_with_nodes)
    with
      | ParsingFailure -> failwith "node lookup exception"


let graph_of_graph: Graph_piqi.graph -> graph = (fun g ->
  let open GameElements in match g with
  | {nodes=ns; edges=es;} ->
    let nodes = List.map node_of_piqinode ns in
    let graph = List.fold_left Map.add_vertex Map.empty nodes in
    let edges = List.map edge_of_piqiedge es in
    let edges_with_nodes = fetch_edge_nodes edges nodes [] in
    List.fold_left (fun g (v1, e, v2) -> Map.add_edge_e g (v1, e, v2) ) graph edges_with_nodes
)

let graph_to_graph: graph -> Graph_piqi.graph = (fun x -> failwith "unimplemented")
