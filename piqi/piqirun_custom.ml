(* Piqi object types *)
open GameElements
open Gameelements_piqi.Vehicle
open Gameelements_piqi.Good
open Player_piqi.Player
open Graph_piqi.Graph

(*TODO: use global helpers, remove dupes *)
let f (x, _, _) = x
let s (_,y,_) = y
let t (_,_,z) = z

exception ParsingFailure

(* Native typedefs need to be repeated manually -- opening the whole module
* overrides the Piqi objects. *)
type vehicle = GameElements.vehicle
type player = Player.player
type graph = GameElements.Map.t
type game_state = GameElements.game_state

(* Conversion functions between Piqi objects and native OCaml objects *)

(* Resource *)
let resource_of_piqiresource pres = 
  let open GameElements in match pres with
  | `lumber -> Lumber
  | `iron -> Iron
  | `oil -> Oil
  | `electronics -> Electronics
  | `produce -> Produce

let resource_to_piqiresource res = 
  match res with
  | Lumber -> `lumber
  | Iron -> `iron
  | Oil -> `oil
  | Electronics -> `electronics
  | Produce -> `produce

(* Vehicle *)
let vehicle_of_vehicle: Gameelements_piqi.vehicle -> GameElements.vehicle =
  (fun pv -> 
    let open GameElements in match pv with
    | {owner=o; t=t; speed=sp; capacity=cap; cargo=cg; age=age'; status=st;
      x=x'; y=y'; destination=dst; loc=vloc} ->
      let t' = match t with 
        | `car -> Car
        | `truck -> Truck
      in
      let cg' = match cg with
        | {t=t; quant=q;} -> 
          let t' = resource_of_piqiresource t in 
          Some {t=t'; quantity=q}
      in
      let st' = match st with 
        | `waiting -> Waiting
        | `driving -> Driving
        | `broken -> Broken
      in
      let vloc' = match vloc with
        | `some i -> Some i
        | `none -> None
      in 
      {
        v_owner_id = o;
        v_t = t';
        v_loc = vloc';
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
let vehicle_to_vehicle: GameElements.vehicle -> Gameelements_piqi.vehicle =
  (fun v -> 
    match v with
    | {v_owner_id=o; v_t=t; v_loc=loc; speed=sp; capacity=cap; cargo=cg;
      age=age'; status=st; x=x'; y=y'; destination=dst;} ->
      let t' = match t with 
        | Car -> `car
        | Truck -> `truck
      in
      let cg' = match cg with
        | Some {t=t; quantity=q;} -> 
          let t' = resource_to_piqiresource t in 
          {t=t'; quant=q}
      in
      let st' = match st with 
        | Waiting -> `waiting
        | Driving -> `driving
        | Broken -> `broken
      in
      let loc' = match loc with
        | Some i -> `some i 
        | None -> `none
      in
      {
        owner = o;
        t = t';
        loc = loc';
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

(* Player *)
let player_of_player: Player_piqi.player -> Player.player = (fun p -> 
  let open Player in match p with
  | {pid=id; ptype=pt; money=m;} ->
    let pt' = match pt with
      | `human -> Human
      | `ai x -> AI x
    in
    {
      p_id = id;
      p_type = pt';
      money = m;
    }
)
let player_to_player: Player.player -> Player_piqi.player = (fun p ->
  match p with
  | {p_id=id'; p_type=pt; money=m;} -> 
    let pt' = match pt with
      | Human -> `human
      | AI x -> `ai x
    in
    {
      pid = id';
      ptype = pt';
      money = m;
    }
)

(* Goods Profile *)
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
let goodsprofile_to_piqigoodsprofile: GameElements.goods_profile -> Gameelements_piqi.Goodsprofile.t =
(fun goodsprofile ->
  match goodsprofile with
  | {resource=r; steps_to_inc=sti; current=curr; capacity=cap; price=p;
    natural_price=np} -> 
    {
      resource = resource_to_piqiresource r;
      stepstoinc = sti;
      current = curr;
      capacity = cap;
      price = p;
      naturalprice = np;
    }
)

(* Graph *)
let node_of_piqinode: Gameelements_piqi.Location.t -> GameElements.location = 
(fun pnode -> 
  let open GameElements in match pnode with
  | {id=id'; lx=lx'; ly=ly'; accepts=a; produces=p} ->
    let a' = List.map goodsprofile_of_piqigoodsprofile a in
    let p' = List.map goodsprofile_of_piqigoodsprofile p in
    {
      l_id = id';
      l_x = lx';
      l_y = ly';
      accepts = a';
      produces = p';
    }
)
let node_to_piqinode: GameElements.location -> Gameelements_piqi.Location.t =
(fun node ->
  match node with
  | {l_id=id'; l_x=lx'; l_y=ly'; accepts=a; produces=p} ->
    let a' = List.map goodsprofile_to_piqigoodsprofile a in
    let p' = List.map goodsprofile_to_piqigoodsprofile p in
    {
      id = id';
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
      c_age = age';
      c_speed = sp;
      length = l;
    }
)
let edge_to_piqiedge: GameElements.connection -> Gameelements_piqi.Connection.t =
(fun edge ->
  match edge with
  | {c_owner_id=o; l_start=st; l_end=end'; c_age=age; c_speed=sp; length=l} ->
    {
      owner = o;
      lstart = st;
      lend = end';
      age = age;
      speed = sp;
      length = l;
    }
)

(* helper to get edge by id *)
let rec fetch_edge_by_id nodes id =
  let open GameElements in match nodes with
  | [] -> raise ParsingFailure
  | h::t -> if h.l_id = id then h else fetch_edge_by_id t id

(* helper to get all node objects for edges *)
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
    List.fold_left (fun g (v1, e, v2) -> Map.add_edge_e g (v1, e, v2) )
      graph edges_with_nodes
)

let graph_to_graph: graph -> Graph_piqi.graph = (fun g ->
  let nodes = Map.fold_vertex (fun nd acc ->
      (node_to_piqinode nd)::acc) g [] in
  let edges = Map.fold_edges_e (fun e_triple acc -> 
      (edge_to_piqiedge (s e_triple))::acc) g [] in
  {
    directed = false;
    type_ = "2D undirected graph";
    label = "Game Map";
    metadata = {var = "str"};
    nodes = nodes;
    edges = edges;
  }
)