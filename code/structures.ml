(***********
 * utility *
 ***********)

exception NotFound of string;;
exception Todo;;

let tuple_first (a, b) = a;;
let tuple_second (a, b) = b;;

let list_assoc_replace lst key newval =
  (key, newval) :: (List.remove_assoc key lst)
;;

let rec list_assoc_merge old_l new_l =
  match new_l with
  | [] -> old_l
  | (k,v)::t -> list_assoc_merge (list_assoc_replace old_l k v) t
;;

let list_count f lst =
  List.length (List.filter f lst)
;;

let list_fun_combine lst =
  List.fold_right
    (fun f acc -> (fun x -> f (acc x)))
    lst
    (fun x -> x);;

(* list of tuples to tuple of list *)
let lot_to_tol2 lst =
  let rec lot_to_tol_rec sub (l1, l2) =
    match sub with
    | [ ] -> (l1, l2);
    | (a, b)::t -> lot_to_tol_rec t (l1@[a], l2@[b])
  in
    lot_to_tol_rec lst ([], [])
;;
let lot_to_tol3 lst =
  let rec lot_to_tol_rec sub (l1, l2, l3) =
    match sub with
    | [ ] -> (l1, l2, l3);
    | (a, b, c)::t -> lot_to_tol_rec t (l1@[a], l2@[b], l3@[c])
  in
    lot_to_tol_rec lst ([], [], [])
;;
let lot_to_tol4 lst =
  let rec lot_to_tol_rec sub (l1, l2, l3, l4) =
    match sub with
    | [ ] -> (l1, l2, l3, l4);
    | (a, b, c, d)::t -> lot_to_tol_rec t (l1@[a], l2@[b], l3@[c], l4@[c])
  in
    lot_to_tol_rec lst ([], [], [], [])
;;

let lot_tol_test = lot_to_tol3 [ (1, 'a', 3) ; (4, 'b', 6) ] ;;
(* given a function that returns identifiers for elements of a list,
   return the next biggest identifier *)
let next_id lst f =
  1 + (List.fold_left max 0 (List.map f lst))
;;



(**************
 * base types *
 **************)
type duration = int;;
type position = float;;

(* a "None" optional duration means infinite: *)
let is_infinite t = match t with
  | None -> true ;
  | _ -> false;;



(*************
 * data tree *
 *************)

type value =
  | Float of float
  | Int of int
  | Bool of bool
  | String of string
  | Audio of float array array;;

type node = { name: string; param: value option; children: node list; };;


(***************
 * environment *
 ***************)

exception UndefinedVariable;;
let noenv = fun v -> raise UndefinedVariable;;
let noenv_opt = fun v -> None;;

type env = string -> value;;

type environment = {
  local: (string * value) list;
  global: (string * value) list
};;
let empty_env = {
  local = [];
  global = []
};;

let pull var e =
  match (List.assoc_opt var e.local) with
  | None -> List.assoc var e.global
  | Some v -> v
;;
let push_local var va e =
  { e with
    local = list_assoc_replace e.local var va
};;
let push_global var va e =
  { e with
    global = list_assoc_replace e.global var va
};;
(** in the C++ code this function applies
    the data recorded in the local environment to the global environment,
    write buffers to the sound card, etc etc *)
let commit e = {
    local = [];
    global = list_assoc_merge e.global e.local
};;



(***************
 * expressions *
 ***************)
type impulseId = ImpulseId of int;;

type expr_listener = {
    exprAddr: string;
    exprSet: bool
};;

type subexpr = Var of string | Value of value;;
type expression =
  | Greater   of subexpr*subexpr
  | GreaterEq of subexpr*subexpr
  | Lower     of subexpr*subexpr
  | LowerEq   of subexpr*subexpr
  | Equal     of subexpr*subexpr
  | Different of subexpr*subexpr
  | Negation  of expression
  | And       of expression*expression
  | Or        of expression*expression
  | Impulse   of impulseId*string
;;

exception EvalError;;

let rec evaluate expr (e:environment) (listeners:(impulseId*expr_listener) list) =
 let compare v1 v2 f =
   match (v1, v2) with
     | (Bool b1, Bool b2) -> f b1 b2
     | _,_ -> raise EvalError
 in
 let eval_atom v1 v2 f e =
   match (v1,v2) with
     | (Var n1, Var n2)     -> compare (pull n1 e) (pull n2 e) f
     | (Var n1, Value n2)   -> compare (pull n1 e) n2 f
     | (Value n1, Var n2)   -> compare n1 (pull n2 e) f
     | (Value n1, Value n2) -> compare n1 n2 f
 in
 match expr with
  | Greater (e1,e2)   -> eval_atom e1 e2 (>) e
  | GreaterEq (e1,e2) -> eval_atom e1 e2 (>=) e
  | Lower (e1,e2)     -> eval_atom e1 e2 (<) e
  | LowerEq (e1,e2)   -> eval_atom e1 e2 (<=) e
  | Equal (e1,e2)     -> eval_atom e1 e2 (=) e
  | Different (e1,e2) -> eval_atom e1 e2 (<>) e
  | Negation e1       -> not (evaluate e1 e listeners)
  | And (e1,e2)       -> (evaluate e1 e listeners) && (evaluate e2 e listeners)
  | Or (e1,e2)        -> (evaluate e1 e listeners) || (evaluate e2 e listeners)
  | Impulse (id,str)  -> (List.assoc id listeners).exprSet

(* env: need to know if a message was received on an address ? *)
(* have a "listener" data structure: elements can insert listeners on the
  "l-env", the l-env is updated at each tick with a value set to true if a value was received *)

(* two useful expressions *)
let true_expression = Equal (Value (Bool true), Value (Bool true));;
let false_expression = Equal (Value (Bool true), Value (Bool false));;


type status = Waiting | Pending | Happened | Disposed;;



(**************
 * data model *
 **************)
type edgeId = EdgeId of int;;
type nodeId = NodeId of int;;
type portId = PortId of int;;
(* ports *)
type edgeType =
    Glutton
  | Strict
  | DelayedGlutton of value list
  | DelayedStrict of value list
;;
type edge = {
    edgeId: edgeId;
    source: portId;
    sink: portId;
    edgeType: edgeType;
};;
type port = {
    portId: portId;
    portAddr: string option;
    portEdges: edgeId list;
};;

type curve = float -> float;;

(* some specific nodes *)
type automation = port * curve;;
type mapping = port * port * curve;;
type sound = port * float array array;;
type passthrough = port * port;;

(* tokens *)
type token_request = {
  token_date: duration;
  position: position;
  offset: duration;
};;

let make_token dur pos off =
  { token_date = dur;
    position = pos;
    offset = off;
  };;

(* nodes of the data graph *)
type dataNode =
    Automation of automation
  | Mapping of mapping
  | Sound of sound
  | Passthrough of passthrough;;
type grNode = {
    nodeId: nodeId;
    data: dataNode;
};;

type graph = {
    nodes: grNode list;
    edges: edge list;
};;

type grNodeState = {
  executed: bool;
  prev_date: duration;
  tokens: token_request list
};;
type graph_state = {
    node_state: (nodeId * grNodeState) list;
    port_state: (portId * value option) list
};;

(* utilities and tests *)
let create_graph = { nodes = []; edges = [] } ;;

let next_node_id lst = NodeId (next_id lst (fun n -> let (NodeId id) = n.nodeId in id));;
let next_edge_id lst = EdgeId (next_id lst (fun n -> let (EdgeId id) = n.edgeId in id));;

let create_port = { portId = PortId 0; portAddr = None; portEdges = []; } ;;

(* get a new port id for when adding nodes to the graph *)
let last_port_id graph =
  let list_nodes_id =
    List.map (fun node ->
        match node.data with
        | Automation (ip, _) -> ip.portId ;
        | Mapping (ip, op, _) -> max ip.portId op.portId ;
        | Sound (op, _) -> op.portId ;
        | Passthrough (ip, op) -> max ip.portId op.portId ;
      ) graph.nodes in
  let (PortId pid) = List.fold_left max (PortId 0) list_nodes_id
  in pid
;;

(* add a node to the data graph *)
let add_node gr nodeDat =
  let update_port_id vp new_id = { vp with portId = (PortId new_id) } in

  (* each port is given an identifier to which each cable can connect to *)
  let new_id = next_node_id gr.nodes in
  let last_port_id = last_port_id gr in

  let newNodeDat = match nodeDat with
    | Automation (ip, curve) -> Automation (update_port_id ip (last_port_id + 1), curve) ;
    | Mapping (ip, op, curve) -> Mapping (update_port_id ip (last_port_id + 1), update_port_id ip (last_port_id + 2), curve) ;
    | Sound (op, audio) -> Sound (update_port_id op (last_port_id + 1), audio) ;
    | Passthrough (ip, op) -> Passthrough (
        update_port_id ip (last_port_id + 1),
        update_port_id op (last_port_id + 2));
  in
  let new_node = { nodeId = new_id; data = newNodeDat; } in
  (new_node, { gr with nodes = new_node::gr.nodes; })
;;

let remove_node l nodeId =
  List.filter (fun x -> x.nodeId <> nodeId) l
;;

(* add an edge between two ports of the data graph *)
let add_edge gr src snk t =
  let new_id = next_edge_id gr.edges in
  let new_edge = { edgeId = new_id; source = src; sink = snk; edgeType = t } in
  (new_edge, { gr with edges = new_edge::gr.edges })
;;

(* find a node in a graph by id *)
let find_node graph nodeId =
  List.find (fun n -> n.nodeId = nodeId) graph.nodes
;;
let find_node_state graph nodeId =
  List.assoc nodeId graph.node_state
;;
let find_edge graph edgeId =
  List.find (fun n -> n.edgeId = edgeId) graph.edges
;;

(* find a port in a graph by id *)
exception PortNotFound;;

let get_all_ports graph =
  let rec impl nodes cur =
  match nodes with
  | [ ] -> cur
  | h :: t ->
    match h.data with
    | Automation (op, _) -> impl t (op::cur)
    | Mapping (ip, op, _) -> impl t (ip::op::cur)
    | Sound (op, _) -> impl t (op::cur)
    | Passthrough (ip, op) ->  impl t (ip::op::cur)
  in
  impl graph.nodes []
;;

let find_port graph portId =
  let rec impl portId nodes =
  match nodes with
  | [ ] -> raise PortNotFound
  | h :: t ->
    match h.data with
    | Automation (op, _) -> if op.portId = portId then op
                            else raise PortNotFound;
    | Mapping (ip, op, _) ->  if ip.portId = portId then ip
                         else if op.portId = portId then op
                         else raise PortNotFound;
    | Sound (op, _) -> if op.portId = portId then op
                       else raise PortNotFound;
    | Passthrough (ip, op) -> if ip.portId = portId then ip
                         else if op.portId = portId then op
                         else raise PortNotFound;
  in
  impl portId graph.nodes
;;

let find_port_state graph portId =
  List.assoc portId graph.port_state
;;
(* find the node of a port in a graph *)
let find_port_node graph portId =
  let rec impl portId nodes =
  match nodes with
  | [ ] -> raise PortNotFound
  | h :: t ->
    match h.data with
    | Automation (op, _) -> if op.portId = portId then h
                            else raise PortNotFound;
    | Mapping (ip, op, _) -> if ip.portId = portId || op.portId = portId then h
                             else raise PortNotFound;
    | Sound (op, _) -> if op.portId = portId then h
                       else raise PortNotFound;
    | Passthrough (ip, op) -> if ip.portId = portId || op.portId = portId then h
                              else raise PortNotFound;
  in
  impl portId graph.nodes
;;

(* replace a node in a graph *)
let replace_node graph nodeId newNode =
  { graph with
    nodes = List.map (fun n -> if n.nodeId = nodeId then newNode else n) graph.nodes;
  };;
let replace_node_state graph nodeId newNode =
  { graph with
    node_state = list_assoc_replace graph.node_state nodeId newNode
  };;

let graph_ident g = g;;

(* register a tick to a node in the graph and return the new graph *)
let add_tick_to_node nodeId token (gs:graph_state) =
  (* tokens go from oldest to newest *)
  let cur_state = (find_node_state gs nodeId) in
  let new_state = {
    cur_state with
    tokens = cur_state.tokens @ [ token ];
  } in
  replace_node_state gs nodeId new_state
;;



(*****************************
 * graph execution functions *
 *****************************)

(* apply a list of functions to the state of the graph *)
let update_graph fun_list graph =
  let apply_rev_fun g f = f g in
  List.fold_left apply_rev_fun graph fun_list ;;

let is_enabled n = (List.length n.tokens) = 0;;

(* todo : disables all nodes which are in strict relationship with a disabled node. *)
let disable_strict_nodes nodes gs =
  gs;;

(* todo : topologically sorted list of nodes *)
let topo_sort graph =
  graph.nodes;;

(* todo : when a node can execute *)
let can_execute nodes = true;;

let replace_value p gs v = { gs with port_state = list_assoc_replace gs.port_state p.portId v };;
(* remove the data stored in a port *)
let clear_port p gs = replace_value p gs None;;

let get_edges edges gr =
  List.find_all (fun x -> List.mem x.edgeId edges) gr.edges;;

(* some combination function *)
let combine (v1:value) (v2:value) =
  match (v1, v2) with
  | (Audio a1, Audio a2) -> Audio a1 (* mix audio *)
  | (_,v) -> v;; (* take latest *)

(* reads the data of the source when there is a cable between two ports *)
let get_src_value edge gr gs =
  let src_val = find_port_state gs edge.source in
  match edge.edgeType with
  | Strict -> src_val
  | Glutton -> src_val
  (* these have their own buffer line where data is copied *)
  | DelayedStrict dl -> raise Todo;
  | DelayedGlutton dl -> raise Todo;
;;

(* combine read data with existing data in a port *)
let aggregate_data gr gs (v: value option) edge  =
  match (v, get_src_value edge gr gs) with
  | (None, None) -> None
  | (None, v) -> v
  | (v, None) -> v
  | (Some v1, Some v2) -> Some (combine v1 v2)
;;

(* copy data from the cables & environment to the port *)
let init_port (p:port) g gs (e:environment) =
  match p.portEdges with
  (* no edges: read from the env *)
  | [] -> let pv = match p.portAddr with
                | None -> None
                | Some str -> Some (pull str e)
                in
         replace_value p gs pv
  (* edges: read from them *)
  | _ -> replace_value p gs (List.fold_left (aggregate_data g gs) None (get_edges p.portEdges g) )
  ;;

(* this sets-up a node for before its execution *)
let init_node n g gs (e:environment) =
  (* clear the outputs of the node *)
  (* and copy data from the environment or edges to the inputs *)
  match n.data with
    | Automation (op, curve)  -> clear_port op gs;

    | Mapping (ip, op, curve) -> let gs = clear_port op gs in
                                 init_port ip g gs e

    | Sound (op, audio)       -> clear_port op gs;

    | Passthrough (ip, op)    -> let gs = clear_port op gs in
                                 init_port ip g gs e
;;

(* actual implementation of the execution algorithm for each kind of process... not really relevant *)
let exec_node_impl data gs token =
  gs
;;
let exec_node g gs n ns token =
  let gs = exec_node_impl n.data gs token in
  { gs with
    node_state = list_assoc_replace gs.node_state n.nodeId {
                      ns with
                      executed = true;
                      prev_date = token.token_date;
                    }
};;

let in_port_disabled edge graph gs =
  not (is_enabled (find_node_state gs (find_port_node graph (find_edge graph edge).sink).nodeId));;

let write_port_env p gs e =
  match (p.portAddr, (find_port_state gs p.portId)) with
  | (Some addr, Some var) -> (push_local addr var e)
  | _ -> e
;;
let write_port_edges p g =
  g (*todo*)
;;

(* write the data in a port to the cables or environment if available *)
let write_port p (g:graph) (gs:graph_state) (e:environment) =
  let has_targets = (p.portEdges = []) in
  let all_targets_disabled =
    has_targets &&
    List.for_all (fun x -> in_port_disabled x g gs) p.portEdges in
  if(not has_targets || all_targets_disabled) then
    (gs, write_port_env p gs e)
  else
    (write_port_edges p gs, e)
;;

(* clear the inputs of a node, and copy its outputs to the environment & delay lines *)
let teardown_node n g gs e =
  match n.data with
  | Automation (op, _)      -> write_port op g gs e;
  | Sound (op, _)           -> write_port op g gs e;
  | Mapping (ip, op, curve) -> let (gs, e) = write_port op g gs e in
                               (clear_port ip gs, e);
  | Passthrough (ip, op)    -> let (gs, e) = write_port op g gs e in
                               (clear_port ip gs, e);
;;

(* todo *)

(* any input has an edge going in *)
let has_port_input node = true;;

(* any input has an address present in the local scope *)
let has_local_input node = true;;

(* any input has an address present in the global scope *)
let has_global_input node = true;;

(* remove all the tokens of all the nodes *)
let clear_tokens (graph:graph_state) =
  let rec impl before after =
    match before with
    | [ ] -> { graph with node_state = after }
    | (id, st) :: t -> impl t ((id, {st with tokens = [ ]}) :: after)
  in
    impl graph.node_state [ ]
;;

(* finds which element occurs at the earliest in a list *)
let rec find_first sorted_nodes n1 n2 =
  match sorted_nodes with
  | [ ] -> raise (Failure "can't happen");
  | h :: t-> if h = n1 then
      1
    else if h = n2 then
      -1
    else find_first t n1 n2;;

let nodes_sort sorted_nodes n1 n2 =
  let p1 = has_port_input n1 in
  let p2 = has_port_input n2 in
  let l1 = has_local_input n1 in
  let l2 = has_local_input n2 in
  let g1 = has_global_input n1 in
  let g2 = has_global_input n2 in
  if p1 && not p2 then
    1
  else if not p1 && p2 then
    -1
  else if p1 && p2 then
    find_first sorted_nodes n1 n2

  else if l1 && not l2 then
    1
  else if not l1 && l2 then
    -1
  else if l1 && l2 then
    find_first sorted_nodes n1 n2

  else if g1 && not g2 then
    1
  else if not g1 && g2 then
    -1
  else
    find_first sorted_nodes n1 n2
;;

let rec sub_tick graph gs nodes (e:environment) =
  match nodes with
  | [ ] -> (gs, e);
  | _ ->
    (* look for all the nodes that can be executed at this point *)
    let next_nodes = List.filter can_execute nodes in

    (* order them and run the first one *)
    let next_nodes = List.sort (nodes_sort next_nodes) next_nodes in
    match next_nodes with
    | [ ] -> (gs, e) ;
    | cur_node::q ->
      (* set it up by copying data to its inputs *)
      let gs = init_node cur_node graph gs e in

      (* execute all the sub-ticks for this node *)
      (* note: actually it's ok to reuse the same environment: it is only used for reading *)
      let rec run_ticks_for_node g gs n tokens =
        match tokens with
        | [] -> gs
        | token::t -> let gs = exec_node g gs n (List.assoc n.nodeId gs.node_state) token
                      in run_ticks_for_node g gs n t
      in
      let gs = run_ticks_for_node graph gs cur_node (List.assoc cur_node.nodeId gs.node_state).tokens in

      (* clear its inputs and copy its outputs to the environment or delay lines if relevant *)
      let (gs, e) = teardown_node cur_node graph gs e in

      (* repeat on the updated graph, with the remaining nodes *)
      sub_tick
        graph
        gs
        (remove_node next_nodes cur_node.nodeId)
        e ;;

let get_enabled_nodes g gs =
  let en = List.filter (fun (id,{ tokens = t }) -> (List.length t) <> 0) gs.node_state in
  let (ids, _) = List.split en in
  List.find_all (fun x -> List.exists (fun y -> y = x.nodeId) ids) g.nodes;;

let add_missing graph gs =
  let rec add_missing_nodes gs nodes =
  match nodes with
  | [] -> gs
  | node::t -> if List.mem_assoc node.nodeId gs.node_state
               then
                add_missing_nodes gs t
               else
                add_missing_nodes { gs with node_state =
                    (node.nodeId, {
                        prev_date = 0;
                        tokens = [];
                        executed = false;
                        }) :: gs.node_state
                } t
  in
  let rec add_missing_ports gs ports =
  match ports with
  | [] -> gs
  | port::t -> if List.mem_assoc port.portId gs.port_state
               then
                add_missing_ports gs t
               else
                add_missing_ports { gs with port_state =
                    (port.portId, None) :: gs.port_state
                } t
  in
  add_missing_nodes (add_missing_ports gs (get_all_ports graph)) graph.nodes

let tick_graph_topo graph gs e =

  (* we mark the nodes which had tokens posted to as enabled *)
  let gs = disable_strict_nodes (get_enabled_nodes graph gs) gs in
  let enabled_nodes = (get_enabled_nodes graph gs) in
  let sorted_nodes = topo_sort graph in
  let filtered_nodes = List.filter (fun n -> (List.mem n enabled_nodes)) sorted_nodes in

  (* we have a set of nodes that we now run: *)
  let (gs, e) = sub_tick graph gs filtered_nodes e in

  (* once all the nodes are ran, remove their tokens *)
  (clear_tokens gs, e);;




(******************************
 * temporal model definitions *
 ******************************)
type intervalId = IntervalId of int;;
type tempCondId = TempCondId of int;;
type instCondId = InstCondId of int;;
type processId = ProcessId of int;;

exception WrongProcess;;
type score_state =
{
 itv_dates: (intervalId * duration) list;
 ic_statuses: (instCondId * status) list;
 listeners: (impulseId*expr_listener) list;
 rootTCs: (processId * tempCondId list) list;
 scoreEnv: environment
};;

let print_dates itv_dates =
  let rec impl list =
    match list with
     | [] -> ()
     | (IntervalId id, date)::t -> Printf.printf "(Interval %i, %i); " id date ; impl t;
  in
    Printf.printf "[" ;
    impl itv_dates ;
    Printf.printf "]\n" ;;

let print_roots tcs =
  let rec impl list =
    match list with
     | [] -> ()
     | (_, lst)::t -> match lst with
                       | [] -> impl t ;
                       | (TempCondId id)::tc_t -> Printf.printf "Root TC %i; " id ; impl t;
  in
    Printf.printf "[" ;
    impl tcs ;
    Printf.printf "]\n" ;;
let print_state (state:score_state) =
  print_dates state.itv_dates;
  print_roots state.rootTCs;
  ;;
type processImpl =
    Scenario of scenario | Loop of loop | DefaultProcess
and process = {
  procId: processId;
  procNode: nodeId;
  impl: processImpl;
  start: process -> score_state -> score_state;
  stop: process -> score_state -> score_state;
  tick: process -> duration -> duration -> position -> duration -> score_state -> ((graph_state -> graph_state) * score_state)
}
and interval = {
  itvId: intervalId;
  itvNode: nodeId;
  minDuration: duration;
  maxDuration : duration option;
  nominalDuration : duration;
  speed: float;
  processes: process list
}
and condition = {
  icId: instCondId;
  condExpr: expression;
  previousItv: intervalId list;
  nextItv: intervalId list;
}
and temporalCondition = {
  tcId: tempCondId;
  syncExpr: expression;
  conds: condition list
}
and scenario = {
  intervals: interval list ;
  tempConds: temporalCondition list;
}
and loop = {
  pattern: interval;
  startTC: temporalCondition;
  endTC: temporalCondition;
};;

(* listeners *)
let register_listener (id, var) lst =
    if List.mem_assoc id lst
    then
        lst
    else
        let new_l = {
            exprAddr = var;
            exprSet = false
        } in list_assoc_replace lst id new_l
;;

let register_listeners (expr:expression) lst =
  let rec walk_imp lst expr =
     match expr with
      | Negation e1       -> (walk_imp lst e1)
      | And (e1,e2)       -> walk_imp (walk_imp lst e1) e2
      | Or (e1,e2)        -> walk_imp (walk_imp lst e1) e2
      | Impulse (id, var) -> register_listener (id, var) lst
      | _                 -> lst
  in
  walk_imp lst expr
;;
let unregister_listeners (expr:expression) lst =
  let rec walk_imp lst expr =
     match expr with
      | Negation e1       -> (walk_imp lst e1)
      | And (e1,e2)       -> walk_imp (walk_imp lst e1) e2
      | Or (e1,e2)        -> walk_imp (walk_imp lst e1) e2
      | Impulse (id, var) -> List.remove_assoc id lst
      | _                 -> lst
  in
  walk_imp lst expr
;;


(* utility functions to work with scenario *)
let find_prev_IC itv scenario =
  let conditions = List.concat (List.map (fun t -> t.conds) scenario.tempConds) in
  List.find (fun ic ->
      (List.exists (fun id -> id = itv.itvId) ic.nextItv)
    )
    conditions
;;

let get_date (itv:interval) (dates:(intervalId * duration) list) =
  match List.assoc_opt itv.itvId dates with
  | None -> 0
  | Some t -> t
;;
let set_date (itv:interval) date (dates:(intervalId * duration) list) =
  list_assoc_replace dates itv.itvId date
;;

set_date {
  itvId = IntervalId 0;
  itvNode = NodeId 0;
  minDuration = 3000;
  maxDuration = Some 3000;
  nominalDuration = 3000;
  speed = 1.;
  processes = [ ];
} 7000 [ (IntervalId 1, 1000) ] ;;

let set_ic_status (ic:instCondId) status (statuses:(instCondId * status) list) =
  list_assoc_replace statuses ic status
;;

let find_next_IC itv scenario =
  let conditions = List.concat (List.map (fun t -> t.conds) scenario.tempConds) in
  List.find (fun ic -> (List.exists (fun id -> id = itv.itvId) ic.previousItv)) conditions
;;

let find_parent_TC cond scenario =
  List.find (fun x -> (List.mem cond x.conds)) scenario.tempConds
;;

let find_end_TC itv scenario =
  find_parent_TC (find_next_IC itv scenario) scenario
;;

let following_intervals cond scenario =
  List.find_all (fun x -> (List.mem x.itvId cond.nextItv)) scenario.intervals
;;

let get_all_ICs scenario =
  let rec impl tc_list cur_ics =
    match tc_list with
    | [] -> cur_ics
    | tc::t -> impl t (tc.conds@cur_ics)
  in
  impl scenario.tempConds []
;;

let add_process interval proc =
  { interval with processes = proc::interval.processes }
;;

let get_intervals id_list scenario =
  List.filter (fun x -> List.mem x.itvId id_list) scenario.intervals;;
let get_temporalConds id_list scenario =
  List.filter (fun x -> List.mem x.tcId id_list) scenario.tempConds;;


(* when a scenario starts, we look up all the "root" temporal conditions :
   they are not preceded by any interval and start on the condition "true" *)
let get_rootTempConds pid scenario state =
  let ids = List.assoc pid state.rootTCs in
  get_temporalConds ids scenario;;

let is_interval_running scenario ic_status itv =
  (List.assoc (find_prev_IC itv scenario).icId ic_status) = Happened &&
  (List.assoc (find_next_IC itv scenario).icId ic_status) <> Happened &&
  (List.assoc (find_next_IC itv scenario).icId ic_status) <> Disposed;;

(*****************************
 * temporal model algorithms *
 *****************************)

(* These functions tick the temporal graph. They produce a pair :
   (new object, function to call on the data graph)
*)

let is_root tc =
  (tc.syncExpr = true_expression)
  &&
  (List.for_all (fun c -> c.previousItv = [ ]) tc.conds)
;;

(*
For processes, add a first tick at t=0 when starting them.
No samples will be produced so the offset does not matter.
*)
let start_process p (state:score_state) =
    (p.start p state,
    add_tick_to_node p.procNode (make_token 0 0. 0)
  )
;;

let stop_process p (state:score_state) =
    p.stop p state
;;
let tick_process cur_date new_date new_pos offset (state:score_state) p  =
   p.tick p cur_date new_date new_pos offset state
;;


  (* ticking of intervals: aggregate all the ticks of the processes *)
let tick_interval (itv:interval) t offset (state:score_state) =
  let (cur_date:duration) = (get_date itv state.itv_dates) in
  let new_date = (cur_date + (truncate (ceil (float t) *. itv.speed))) in
  let new_pos = (float new_date /. float itv.nominalDuration) in
  let tp = tick_process cur_date new_date new_pos offset in
  let rec exec_processes procs funs state =
    match procs with
    | [] -> (funs, state)
    | proc::t -> let (nf, ns) = tp state proc in
                 exec_processes t (funs@[nf]) ns
  in

  (* execute all the processes *)
  let (funs, state) = exec_processes itv.processes [] state in

  (* execute the interval itself *)
  ({ state with itv_dates = (set_date itv new_date state.itv_dates) },
   (funs @ [ add_tick_to_node itv.itvNode (make_token new_date new_pos offset) ]))
;;

let start_interval itv (state:score_state) =
  let rec start_processes procs funs (state:score_state) =
    match procs with
    | [] -> (funs, (state:score_state))
    | proc::t -> let (state, nf) = start_process proc state in
                 start_processes t (funs@[nf]) state
  in

  let (funs, state) = start_processes itv.processes [] state in
  ({ state with itv_dates = list_assoc_replace state.itv_dates itv.itvId 0 },
   (funs @ [ add_tick_to_node itv.itvNode (make_token 0 0. 0) ])
  )
;;

let stop_interval itv = (*todo*)
  itv
;;


(****************
 * default proc *
 ****************)

let default_tick p oldd new_date new_pos offset state =
  (add_tick_to_node p.procNode (make_token new_date new_pos offset), state)
and default_start p state =
  state
and default_stop p state =
  state
;;


(************
 * scenario *
 ************)

(*** utilities ***)


let rec scenario_ic_happen scenario ic =
  (* mark ic as executed, add previous intervals to stop set, next intervals to start set *)
  let started_set = ic.nextItv in
  let stopped_set = ic.previousItv in
  (Happened, started_set, stopped_set)

and scenario_ic_dispose scenario ic =
  (* mark ic as disposed,
     add previous intervals to stop set,
     disable next intervals,
     disable next ics if all of their previous intervals are disabled *)
  let stopped_set = ic.previousItv in
  (Disposed, [ ], stopped_set)

(* this functions ticks an interval in the context of a scenario.
   it returns ( (new_interval, list of functions to apply), overticks )
 *)
and scenario_run_interval scenario overticks tick offset interval (state:score_state) =
  let end_TC = find_end_TC interval scenario in
  let interval_date = (List.assoc interval.itvId state.itv_dates) in

  match interval.maxDuration with
    (* if there is no max, we can go to the whole length of the tick *)
    | None -> (tick_interval interval tick offset state, overticks)

    (* if there is a max, we have to stop at the max and save the remainings *)
    | Some maxdur ->
      let actual_tick = min tick (maxdur - interval_date) in
      let tick_res = tick_interval interval actual_tick offset state in
      let overtick = tick - (maxdur - interval_date) in

      (* find if there was already over-ticks recorded for this TC, and if so, update them *)
      match List.assoc_opt end_TC.tcId overticks with
        | None -> (tick_res, (end_TC.tcId, (overtick, overtick))::overticks)
        | Some (min_ot, max_ot) ->
            let new_overtick = (min overtick min_ot, max overtick max_ot) in
            (tick_res, list_assoc_replace overticks end_TC.tcId new_overtick)

(* this function does the evaluation & execution of a given temporal condition *)
and scenario_process_TC scenario tc (state:score_state) =
  (**** utilities ****)

  (* minDurReached ic = true iff all the non-disposed previous intervals
     have reached their min duration *)
  let minDurReached ic (state:score_state) =
    (* find the intervals in the evaluation area *)
    let min_reached itv =
      ((List.assoc itv.itvId state.itv_dates) >= itv.minDuration) ||
      (List.assoc (find_prev_IC itv scenario).icId state.ic_statuses) = Disposed
    in
    List.for_all min_reached (get_intervals ic.previousItv scenario)
  in

  (* maxDurReached ic = true iff any of the previous intervals
     have reached their max duration *)
  let maxDurReached ic (state:score_state) =
    let max_reached itv =
       match itv.maxDuration with
       | None -> false
       | Some t -> (List.assoc itv.itvId state.itv_dates) >= t
    in
    List.exists max_reached (get_intervals ic.previousItv scenario)
  in

  (* execution of a given instantaneous condition *)
  (* returns (ic, started intervals, stopped intervals *)
  let execute_ic (state:score_state) ic =
    if evaluate ic.condExpr state.scoreEnv state.listeners
    then
      scenario_ic_happen scenario ic
    else
      scenario_ic_dispose scenario ic
  in

  (* execution of a given temporal condition *)
  let execute_tc tc (state:score_state) =
    (* execute the conditions *)
    let rec execute_all_ics ics (state:score_state) started_itvs ended_itvs happened_ics =
      match ics with
      | [] -> (state, started_itvs, ended_itvs, happened_ics)
      | cond::t -> let (newStatus, started, stopped) = execute_ic state cond in
                   execute_all_ics
                    t
                    (* update the statuses of the ICs with new values *)
                    { state with ic_statuses = (list_assoc_replace state.ic_statuses cond.icId newStatus) }
                    (started@started_itvs)
                    (stopped@ended_itvs)
                    (if newStatus = Happened then cond::happened_ics else happened_ics)
    in
    let (state, started_itv_ids, ended_itv_ids, happened_ics) = execute_all_ics tc.conds state [] [] [] in

    (* start and stop the intervals *)
    let rec start_all_intervals itvs (state:score_state) funs =
      match itvs with
      | [] -> (state, funs)
      | itv::t -> let (state,f) = start_interval itv state in
                  start_all_intervals t state (funs@[f])
    in
    let (state, funs) =
        start_all_intervals (get_intervals started_itv_ids scenario) state [] in

(*
    let ended_intervals =
        List.map
            stop_interval
            (get_intervals ended_itv_ids scenario) in
*)
    (state, List.flatten funs, happened_ics)
  in

  (**** actual execution ****)
  (* mark all instantaneous conditions with min reached as Pending *)
  let rec mark_IC_min conds state =
    match conds with
    | [] -> state
    | cond::t -> mark_IC_min
                 t
                 (if (minDurReached cond state)
                   then { state with
                          ic_statuses = list_assoc_replace state.ic_statuses cond.icId Pending }
                   else state)
  in
  let state = mark_IC_min tc.conds state in

  (* amongst all the pending ones, we check if any has reached its max *)
  let tcMaxDurReached =
    List.exists
      (fun ic -> ((List.assoc ic.icId state.ic_statuses) = Pending) && (maxDurReached ic state))
      tc.conds
  in

  let is_pending_or_disposed ic =
    let cur_st = (List.assoc ic.icId state.ic_statuses) in
    cur_st = Pending || cur_st = Disposed
  in
  (* if not all ICs are pending or disposed *)
  if (not (List.for_all is_pending_or_disposed tc.conds))
  then
    ((state, [ ], [ ]), false)
  else
    if ((tc.syncExpr <> true_expression) && (not tcMaxDurReached))
    then
      let state = { state with listeners = register_listeners tc.syncExpr state.listeners } in

      if (not (evaluate tc.syncExpr state.scoreEnv state.listeners))
      then
        (* expression is false, do nothing apart updating the TC *)
        ((state, [ ], [ ]), false)
      else
        let state = { state with listeners = unregister_listeners tc.syncExpr state.listeners } in
        (* the tc expression is true, we can proceed with the execution of what follows *)
        (execute_tc tc state, true)
    else
     (* max reached or true expression, we can execute the temporal condition  *)
     (execute_tc tc state, true)


(*** main functios ***)

and start_scenario (proc:process) (state:score_state) =
   let (s:scenario) = (match proc.impl with | Scenario s -> s | _ -> raise WrongProcess) in
   { state with
     (* find all root TCs *)
     rootTCs = list_assoc_replace
                 state.rootTCs
                 proc.procId
                 (List.map (fun x -> x.tcId) (List.filter is_root s.tempConds));
     (* set the date of every interval to 0 *)
     itv_dates = list_assoc_merge state.itv_dates (List.map (fun x -> (x.itvId, 0)) s.intervals);
     (* set all ICs as waiting *)
     ic_statuses = list_assoc_merge state.ic_statuses (List.map (fun x -> (x.icId, Waiting)) (get_all_ICs s))
   }

and stop_scenario s (state:score_state) =
  state;

and tick_scenario (p:process) olddate newdate pos offset (state:score_state) =
  let pid = p.procId in
  let scenario = match p.impl with | Scenario s -> s | _ -> raise WrongProcess in
  let dur = newdate - olddate in
  (* execute the list of root TCs.
     l1 : list of executed ICs
     l2 : list of resulting functions  *)
  let rec process_root_tempConds tc_list state funs =
    match tc_list with
    | [ ] -> (state, funs)
    | h::t ->
        (* try to execute the TC *)
        let ((state, new_funs, happened_ics), executed) =
            scenario_process_TC scenario h state in

        if (not executed) then
          (* The trigger wasn't executed, we keep it *)
          process_root_tempConds t state (funs@new_funs)
        else
          (* the TC was executed, remove it from the roots *)
          let cur_rootTCs = List.filter (fun x -> x <> h.tcId) (List.assoc pid state.rootTCs) in
          process_root_tempConds
            t
            { state with rootTCs = list_assoc_replace state.rootTCs pid cur_rootTCs }
            (funs@new_funs)
  in

  (* execute a given list of TCs *)
  let rec process_tempConds tc_list (state:score_state) funs happened_ics =
    match tc_list with
    | [ ] -> (state, funs, happened_ics)
    | h::t ->
        (* try to execute the TC *)
        let ((state, new_funs, new_hics), _) =
            scenario_process_TC scenario h state in
        process_tempConds t state (funs@new_funs) (new_hics@happened_ics)
  in

  (* execute a list of intervals *)
  let rec process_intervals itv_list overticks funs dur offset end_TCs (state:score_state) =
    match itv_list with
    | [ ] -> (state, overticks, end_TCs, funs)
    | interval::tail ->
        (* run the interval and replace it in a new scenario *)
        let ((state, new_funs), overticks) =
            scenario_run_interval scenario overticks dur offset interval state in
        process_intervals
         tail overticks
         (funs@new_funs)
         dur offset
         ((find_end_TC interval scenario)::end_TCs)
         state
  in

  let rec finish_tick overticks conds funcs dur offset end_TCs state =
    match conds with
    | [ ] ->
      (* now we can process remaining end_TCs *)
      (match end_TCs with
       (* nothing to execute anymore *)
       | [ ] -> (state, funcs)
       (* some TCs reached their end so we execute them *)
       | _ -> let (state, new_funs, conds) =
                process_tempConds end_TCs state [] [] in
             finish_tick overticks conds (funcs@new_funs) dur offset [ ] state)

    | (cond:condition) :: remaining ->
      (* look if an over-tick was recorded for the TC *)
      match (List.assoc_opt (find_parent_TC cond scenario).tcId overticks) with
      | None -> finish_tick overticks remaining funcs dur offset end_TCs state
      | Some (min_t, max_t) ->
         (* we can go forward with executing some intervals *)
         let (state, overticks, end_TCs, funcs) =
             process_intervals
                (following_intervals cond scenario)
                overticks funcs
                max_t
                (offset + dur - max_t)
                end_TCs
                state
         in
         finish_tick overticks remaining funcs dur offset end_TCs state
  in

  (*** actual execution begins here ***)

  (* first execute the root temporal conditions, if any *)
  let (state, funcs) =
    process_root_tempConds (get_rootTempConds pid scenario state) state [] in

  (* run the intervals that follows them *)
  let running_intervals = (List.filter (is_interval_running scenario state.ic_statuses) scenario.intervals) in
  let (state, overticks, end_TCs, funcs) =
    process_intervals running_intervals [] funcs dur offset [] state in

  (* run potential terminating temporal conditions *)
  let (state, funcs, conds) = process_tempConds end_TCs state funcs [] in

  (* loop until the time cannot be advanced in any branch anymore *)
  let (state, funcs) = finish_tick overticks conds funcs dur offset end_TCs state in
  (list_fun_combine funcs, state)
;;

(*********
 * loop *
 ********)

let tick_loop (p:process) olddate newdate pos offset (state:score_state)  =

  let is_simple_loop loop =
    loop.startTC.syncExpr = true_expression &&
    loop.endTC.syncExpr = true_expression &&
    (List.hd loop.startTC.conds).condExpr = true_expression &&
    (List.hd loop.endTC.conds).condExpr = true_expression
  in


  let pid = p.procId in
  let loop = match p.impl with | Loop l -> l | _ -> raise WrongProcess in
  let tick_amount = newdate - olddate in
  if is_simple_loop loop
  then
    let rec tick_simple tick_amount state =
      if dur > 0 then
        if (get_date loop.interval state) + tick_amount < i
    in
    tick_simple dur state
  else
    (graph_ident, state)

and start_loop l state =
  state;
and stop_loop l (state:score_state) =
  state;;

(*************
 * main loop *
 *************)

(** this simulates the arrival of new data in the global environment :
    audio inputs, etc. **)
let update e ext_events olddate date =
  let new_msgs = ext_events olddate date in
  let rec apply e msgs =
    match msgs with
    | [] -> e
    | (var,v)::t -> apply (push_global var v e) t
  in apply e new_msgs
;;

let rec set_listener var lst =
  let rec set_impl remaining l =
  match remaining with
  | [] -> l
  | (ImpulseId id, { exprAddr = a })::t -> let l =
                                             if var = a then
                                               list_assoc_replace l (ImpulseId id) { exprAddr = a; exprSet = true }
                                             else
                                               l
                                           in set_impl t l
  in set_impl lst []
;;
let update_listeners lst local_e ext_events olddate date =
  let new_msgs = ext_events olddate date in
  let rec apply lst msgs =
    match msgs with
    | [] -> lst
    | (var,v)::t -> apply (set_listener var lst) t
  in
  (* first apply the messages from outside *)
  let lst = apply lst new_msgs in

  (* then the ones in the local env *)
  apply lst local_e

(** overall main loop: run a score for some amount of time,
    at a given granularity (eg tick every 50 units) **)
let main_loop root graph duration granularity (state:score_state) ext_events ext_modifications =
    let total_dur = duration in
    let rec main_loop_rec root graph
                          remaining old_remaining granularity
                          (state:score_state) (gs:graph_state) funs =
      if remaining > 0
      then
        (
        let elapsed = total_dur - remaining in
        let old_elapsed = total_dur - old_remaining in

        (* some external action at this time may change the internal structure *)
        let (root,graph,state) = ext_modifications root graph state old_elapsed elapsed  in

        (* tick the time and get functions to execute *)
        let (state, new_funs)  = tick_interval root granularity 0 state in

        (* create states for nodes if required (eg if they have been added for some reason) *)
        let gs = add_missing graph gs in

        (* run the graph with the applied functions *)
        let (gs, e)            = tick_graph_topo graph (update_graph (funs@new_funs) gs) state.scoreEnv in

        (* update the state with things that may happen outside *)
        let state = { state with
                      scoreEnv = (update (commit e) ext_events old_elapsed elapsed);
                      listeners = (update_listeners state.listeners e.local ext_events old_elapsed elapsed)
        } in

        (* go on to the next tick *)
        main_loop_rec
            root graph
            (remaining - granularity)
            old_remaining
            granularity
            state
            gs
            []
        )
      else
        (root, graph, state)
    in
    let (state, funs) = start_interval root state in
    let gs = { node_state = [] ; port_state = [] } in
    main_loop_rec root graph duration duration granularity state gs funs
;;









(*********
 * tests *
 *********)
let some_env = fun s -> (Bool (true));;


(* quick checks *)
let test_atom = Greater (Value (Float 3.4), Value (Float 4.5));;
let test_comp = Or (test_atom, test_atom);;


(* test *)
let test_edge = { edgeId = EdgeId 33; source = PortId 4; sink = PortId 5; edgeType = Glutton; };;
let some_sound_data = Array.make 2 (Array.make 8 0.);;
let some_sound = Sound (create_port, some_sound_data);;

let some_passthrough = Passthrough ( create_port, create_port );;

(* test *)
let test_node_1 = { nodeId = NodeId 1; data = some_sound;  } ;;
let test_node_2 = { nodeId = NodeId 34; data = some_sound; } ;;
next_node_id [ test_node_1; test_node_2 ] ;;

(* quick checks *)
let test_edge = { edgeId = EdgeId 33; source = PortId 4; sink = PortId 5; edgeType = Glutton; };;
let some_sound_data = Array.make 2 (Array.make 8 0.);;
let some_sound = Sound (create_port, some_sound_data);;

let some_passthrough = Passthrough ( create_port, create_port);;

(* quick checks *)
let test_node_1 = { nodeId = NodeId 1; data = some_sound; };;
let test_node_2 = { nodeId = NodeId 34; data = some_sound; };;
next_node_id [ test_node_1; test_node_2 ] ;;

(* quick checks *)
let test_g = create_graph;;
let (snd1, test_g) = add_node test_g some_sound;;
let (snd2, test_g) = add_node test_g some_sound;;
let (p1, test_g) = add_node test_g some_passthrough;;


let nodes = [
    { nodeId = NodeId 1; data = some_sound; };
    { nodeId = NodeId 2; data = some_sound; }
] in
remove_node nodes (NodeId 1);;

(*
let nodes = [ { nodeId= 1; data = some_sound; executed = false; prev_date = 0; tokens = [ ]}; { nodeId= 2; data = some_sound; executed = false; prev_date = 0; tokens = [ ] } ] in
remove_node nodes 1;;
*)

let test_n = { name = "foo"; param = Some (Float 3.4); children = [] } ;;


(* Complete example: 2-track sequencer *)
(* 1. Create data graph *)

let test_g = create_graph in
let (snd_node_1, test_g) = add_node test_g some_sound in
let (snd_node_2, test_g) = add_node test_g some_sound in
let (itv_node_1, test_g) = add_node test_g some_passthrough in
let (itv_node_2, test_g) = add_node test_g some_passthrough in
let (itv_node_3, test_g) = add_node test_g some_passthrough in
let (sc_node_1, test_g) = add_node test_g some_passthrough in
let (itv_node_4, test_g) = add_node test_g some_passthrough in


(* 2. Create temporal structures *)
let test_itv_1 = {
  itvId = IntervalId 1;
  itvNode = itv_node_1.nodeId;
  minDuration = 4500;
  maxDuration = Some 4500;
  nominalDuration = 4500;
  speed = 1.;
  processes = [
    {
      procId = ProcessId 1;
      procNode = snd_node_1.nodeId;
      impl = DefaultProcess;
      start = default_start;
      stop = default_stop;
      tick = default_tick;
    }
  ];
} in

let test_itv_2 = {
  itvId = IntervalId 2;
  itvNode = itv_node_2.nodeId;
  minDuration = 3000;
  maxDuration = Some 3000;
  nominalDuration = 3000;
  speed = 1.;
  processes = [ ];
} in

let test_itv_3 = {
  itvId = IntervalId 3;
  itvNode = itv_node_3.nodeId;
  minDuration = 5000;
  maxDuration = Some 5000;
  nominalDuration = 5000;
  speed = 1.;
  processes = [
    {
      procId = ProcessId 2;
      procNode = snd_node_2.nodeId;
      impl = DefaultProcess;
      tick = default_tick;
      start = default_start;
      stop = default_stop;
    }
  ];
} in


let test_TC_1 = {
  tcId = TempCondId 1;
  syncExpr = true_expression;
  conds = [ {
      icId = InstCondId 1;
      condExpr = true_expression;
      previousItv = [ ];
      nextItv = [ IntervalId 1 ];
    } ];
} in

let test_TC_2 = {
  tcId = TempCondId 2;
  syncExpr = true_expression;
  conds = [ {
      icId = InstCondId 2;
      condExpr = true_expression;
      previousItv = [ IntervalId 1 ];
      nextItv = [ IntervalId 2 ];
    } ];
} in

let test_TC_3 = {
  tcId = TempCondId 3;
  syncExpr = true_expression;
  conds = [ {
      icId = InstCondId 3;
      condExpr = true_expression;
      previousItv = [ IntervalId 2 ];
      nextItv = [ IntervalId 3 ];
    } ];
} in

let test_TC_4 = {
  tcId = TempCondId 4;
  syncExpr = true_expression;
  conds = [ {
      icId = InstCondId 4;
      condExpr = true_expression;
      previousItv = [ IntervalId 3 ];
      nextItv = [  ];
    } ];
} in


let test_scenario = Scenario {
    intervals = [ test_itv_1; test_itv_2; test_itv_3 ];
    tempConds = [ test_TC_1; test_TC_2; test_TC_3; test_TC_4 ];
  }
in

let test_root = {
  itvId = IntervalId 0;
  itvNode = itv_node_4.nodeId;
  minDuration = 0;
  maxDuration = None;
  nominalDuration = 10000;
  speed = 1.;
  processes = [
    {
      procId = ProcessId 3;
      procNode = sc_node_1.nodeId;
      impl = test_scenario;
      tick = tick_scenario;
      start = start_scenario;
      stop  = stop_scenario;
    }
  ]
} in
let empty_state = { scoreEnv = empty_env; ic_statuses = []; itv_dates = []; listeners = []; rootTCs = [] }in
let glob_env d1 d2 = [] in
let glob_cmds a b c dold dnew = (a,b,c) in
let (a,b,c) = main_loop test_root test_g 7000 1000 empty_state glob_env glob_cmds in
c;;

 (*
in
(* tick_interval test_itv_1 100 0 in *)
let _ =
replace_intervals {
    intervals = [ test_itv_3; test_itv_2;  ];
    tempConds = [ ];
    root_tempConds = [ ];
  } [ {
  itvId = IntervalId 2;
  itvNode = itv_node_2.nodeId;
  minDuration = 1234;
  maxDuration = Some 5678;
  nominalDuration = 3000;
  date = 100; itvStatus = Waiting;
  processes = [ ];
}  ] in

let (ts, f) =
tick_scenario {
    intervals = [ test_itv_1; test_itv_2 ];
    tempConds = [ test_TC_1; test_TC_2; test_TC_3 ];
    root_tempConds = [ test_TC_1.tcId ];
  } 6000 0.1 0 empty_env
  in tick_scenario ts 1000 0.1 0 empty_env ;;
exit 0;;
*)

(*
type environment = {
  local: ((string -> value) * (string -> value -> environment -> environment));
  global: ((string -> value) * (string -> value -> environment -> environment))
};;
let env = 0;;
let temporal_tick_res = tick_interval test_root 100 0 ;;

let env = 0;;
let temporal_tick_res = tick_interval test_root 100 0  in
let test_g = update_graph (tuple_second temporal_tick_res) test_g in
let (test_g, env2) = tick_graph_topo test_g env in

let temporal_tick_res = tick_interval (tuple_first temporal_tick_res) 100 0 in
let test_g = update_graph (tuple_second temporal_tick_res) test_g in
let (test_g, env) = tick_graph_topo test_g env in

let temporal_tick_res = tick_interval (tuple_first temporal_tick_res) 100 0 in
let test_g = update_graph (tuple_second temporal_tick_res) test_g in
let (test_g, env) = tick_graph_topo test_g env in
test_g ;;



*)


(*** example queue de reverb ? ***)
(* fonctionnement: estimateur de gain -> si RMS < -90dB on coupe le trigger.
 ---------(- - - - - T
        reverb
  -------------------
        RMS
  -------------------
*)

(* looper *)
(* scenario avec boucles :

- - - - - - - - - - - T
         loop
   |----------------|
   |                |
   |---|----|       |
   |     S1         |
   |--------|----|  |
   |          S2    |
   |                |
   |---|--------|   |
   |       F1       |
   |                |

S1 -> audio:/out/main
S2 -> audio:/out/main
audio:/out/main -> F1 -> audio:/out/main

*)
