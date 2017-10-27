(* Types de base *)
type duration = int;;
type position = float;;

(* a "None" optional duration means infinite: *)
let is_infinite t = match t with 
  | None -> true ; 
  | _ -> false;;
  
  
(* Données qu'on manipule *)
(* Notamment, node est l'arbre de paramètres qu'on contrôle.
 * Par exemple ça peut modéliser un synthétiseur, un programme externe, etc...
 *)
type value = Float of float | Int of int | Bool of bool | String of string;;
type valueParameter = value;;
type audioParameter = float array array;;
type parameter = ValueParam of valueParameter;;
type node = { name: string; param: parameter option; children: node list; };;

let test_p = ValueParam (Float 3.4) ;;
let test_n = { name = "foo"; param = Some test_p; children = [] } ;;

let pull p = match p with
  | ValueParam v -> v ;;
let push p nv = match p with
  | ValueParam v -> ValueParam nv ;;

(* Définition des expressions à partir de ces paramètres *)
type binop = And | Or | Xor;;
type comparator = Greater | GreaterEq | Lower | LowerEq | Equal | Different ;;
type atomElement = AtomParam of parameter | AtomValue of value ;;
type atom = atomElement * atomElement * comparator
and negation = expression
and composition = expression * expression * binop
and impulse = parameter * bool
and expression = Atom of atom | Negation of negation | Composition of composition | Impulse of impulse ;;

(* Deux expressions utiles *)
let true_expression = Atom ((AtomValue (Bool true)), (AtomValue (Bool true)), Equal);;
let false_expression = Atom ((AtomValue (Bool true)), (AtomValue (Bool true)), Different);;

(* quick checks *)
let test_atom = Atom ((AtomValue (Float 3.4)), (AtomValue (Float 4.5)), Greater);;
let test_comp = Composition (test_atom, test_atom, Or);;

(* TODO mais pas nécessairement pour l'article *)
let evaluate expr = true ;;
let update expr = expr ;;

(* pas utile ici
type message = { messParam: parameter ; messValue: value };;
type state = message list;;
*)

type status = Waiting | Pending | Happened | Disposed;;

(* Modèle de données *)
(* ports *)
type edgeType = Glutton | Strict | Delayed ;;
type edge = { edgeId: int; source: int; sink: int; edgeType: edgeType; }
and audioPort = { audioPortId: int; audioPortAddr: audioParameter option; audioEdges: edge list }
and valuePort = { valuePortId: int; valuePortAddr: valueParameter option; valueEdges: edge list }
and port = AudioPort of audioPort | ValuePort of valuePort
;;

type curve = (float * float) list ;;
let value_at curve x = 0.0;;

(* Des noeuds spécifiques *)
type automation = valuePort * curve;;
type mapping = valuePort * valuePort * curve;;
type sound = audioPort * float array array;;
type passthrough = audioPort * valuePort * audioPort * valuePort;;

(* Noeuds du graphe de données *)
type dataNode = Automation of automation | Mapping of mapping | Sound of sound | Passthrough of passthrough ;;
type token_request = { tokenDate: duration; position: position; offset: duration; start_discontinuous: bool; end_discontinuous: bool; };;
type grNode = { nodeId: int; data: dataNode; executed: bool; prev_date: duration; tokens: token_request list; };;

type graph = { nodes: grNode list ; edges: edge list; };;

(* opérations de création *)
let next_id lst f = 1 + (List.fold_left max 0 (List.map f lst));;
let next_node_id lst = next_id lst (fun n -> n.nodeId);;
let next_edge_id lst = next_id lst (fun n -> n.edgeId);;

let create_audio_port = { audioPortId = 0; audioPortAddr = None; audioEdges = []; } ;;
let create_value_port = { valuePortId = 0; valuePortAddr = None; valueEdges = []; } ;;

let test_edge = { edgeId = 33; source = 4; sink = 5; edgeType = Glutton; };;
let some_sound_data = Array.make 2 (Array.make 8 0.);;
let some_sound = Sound (create_audio_port, some_sound_data);;

let some_passthrough = Passthrough ( create_audio_port, create_value_port, create_audio_port, create_value_port );;

(* test *)
let test_node_1 = { nodeId = 1; data = some_sound; executed = false; prev_date = 0; tokens = [ ]; } ;;
let test_node_2 = { nodeId = 34; data = some_sound; executed = false; prev_date = 0; tokens = [ ]; } ;;
next_node_id [ test_node_1; test_node_2 ] ;;



let create_graph = { nodes = []; edges = [] } ;;

let last_port_id graph =
  let list_nodes_id =
  List.map (fun node ->
    match node.data with
     | Automation (ip, _) -> ip.valuePortId ;
     | Mapping (ip, op, _) -> max ip.valuePortId op.valuePortId ;
     | Sound (op, _) -> op.audioPortId ;
     | Passthrough (ai, vi, ao, vo) -> max (max ai.audioPortId vi.valuePortId) (max ao.audioPortId vo.valuePortId) ;
  ) graph.nodes in
  List.fold_left max 0 list_nodes_id
  ;;

let update_vp_id { valuePortId = id; valuePortAddr = a; valueEdges = e} new_id =
  { valuePortId = new_id; valuePortAddr = a; valueEdges = e };;
let update_ap_id { audioPortId = id; audioPortAddr = a; audioEdges = e} new_id =
  { audioPortId = new_id; audioPortAddr = a; audioEdges = e };;

let add_node gr nodeDat =
  (* each port is given an identifier to which each cable can connect to *)
  let new_id = next_node_id gr.nodes in
  let last_port_id = last_port_id gr in
  let newNodeDat = match nodeDat with
   | Automation (ip, curve) -> Automation (update_vp_id ip (last_port_id + 1), curve) ;
   | Mapping (ip, op, curve) -> Mapping (update_vp_id ip (last_port_id + 1), update_vp_id ip (last_port_id + 2), curve) ;
   | Sound (op, audio) -> Sound (update_ap_id op (last_port_id + 1), audio) ;
   | Passthrough (ai, vi, ao, vo) -> Passthrough (
        update_ap_id ai (last_port_id + 1),
        update_vp_id vi (last_port_id + 2),
        update_ap_id ao (last_port_id + 3),
        update_vp_id vo (last_port_id + 4));
    in
  let new_node = { nodeId = new_id; data = newNodeDat; executed = false; prev_date = 0; tokens = [ ]; } in
  (new_node, {nodes = new_node::gr.nodes; edges = gr.edges})
;;

let add_edge gr src snk t =
  let new_id = next_edge_id gr.edges in
  let new_edge = { edgeId = new_id; source = src; sink = snk; edgeType = t } in
  (new_edge, { nodes = gr.nodes; edges = new_edge::gr.edges })
;;

(* quick checks *)
let test_edge = { edgeId = 33; source = 4; sink = 5; edgeType = Glutton; };;
let some_sound_data = Array.make 2 (Array.make 8 0.);;
let some_sound = Sound (create_audio_port, some_sound_data);;

let some_passthrough = Passthrough ( create_audio_port, create_value_port, create_audio_port, create_value_port );;

(* quick checks *)
let test_node_1 = { nodeId = 1; data = some_sound; executed = false; prev_date = 0; tokens = [ ]; };;
let test_node_2 = { nodeId = 34; data = some_sound; executed = false; prev_date = 0; tokens = [ ]; };;
next_node_id [ test_node_1; test_node_2 ] ;;

(* quick checks *)
let test_g = create_graph;;
let (snd1, test_g) = add_node test_g some_sound;;
let (snd2, test_g) = add_node test_g some_sound;;
let (p1, test_g) = add_node test_g some_passthrough;;

(* let (e1, test_g) = add_edge snd1. *)

let make_token dur pos off = { tokenDate = dur; position = pos; offset = off; start_discontinuous = false; end_discontinuous = false; };;
let push_token token { nodeId = id; data = dn; executed = e; prev_date = pd; tokens = tk; } =
    { nodeId = id; data = dn; executed = e; prev_date = pd; tokens = token::tk; } ;;

(* temporal model definitions *)
type processImpl =
    Scenario of scenario | Loop of loop | None
and process = {
    procNode: int;
    procEnable: bool;
    curTime: duration;
    curOffset: duration;
    curPos: position;
    impl: processImpl;
}
and interval = {
    itvId: int;
    itvNode: int;
    minDuration: duration;
    maxDuration : duration option;
    nominalDuration : duration;
    date : duration;
    itvStatus: status;
    processes: process list
}
and condition = {
    condExpr: expression;
    previousItv: int list;
    nextItv: int list;
    status: status;
}
and temporalCondition = {
    syncExpr: expression;
    conds: condition list
}
and scenario = {
    intervals: interval list ;
    triggers: temporalCondition list;
}
and loop = {
    pattern: interval;
    startTrig: temporalCondition;
    endTrig: temporalCondition;
};;

(* todo *)
let find_prev_condition itv scenario = 
  let conditions = List.concat (List.map (fun t -> t.conds) scenario.triggers) in 
  List.find (fun ic -> (List.exists (fun id -> id == itv.itvId) ic.nextItv)) conditions
;;
let find_next_condition itv scenario = 
  let conditions = List.concat (List.map (fun t -> t.conds) scenario.triggers) in 
  List.find (fun ic -> (List.exists (fun id -> id == itv.itvId) ic.previousItv)) conditions
  ;;
  
let add_process interval proc = match interval with
  | (a,b,c,d,e) -> (a,b,c,d,proc::e);;

(* this supposes that the conditions and triggers are part of the scenario *)
let next_interval_id lst = next_id lst (fun n -> n.itvId);;
let add_interval sc inv = { intervals = inv::sc.intervals; triggers = sc.triggers };;
let add_trigger sc trg = { intervals = sc.intervals; triggers = trg::sc.triggers };;

let tuple_first (a,b) = a;;
let tuple_second (a,b) = b;;

let find_node graph nodeId = List.find (fun n -> n.nodeId == nodeId) graph.nodes;;
let replace_node graph nodeId newNode =
    {nodes = List.map (fun n -> if n.nodeId == nodeId then newNode else n) graph.nodes;
     edges = graph.edges};;

let graph_ident g = g;;
let add_tick_to_node nodeId token graph =
    let node = find_node graph nodeId in
    let new_node = {
        nodeId = node.nodeId;
        data =  node.data;
        executed = node.executed;
        prev_date = node.prev_date;
        tokens = node.tokens @ [ token ]; (* tokens go from oldest to newest *)
    } in
    replace_node graph nodeId new_node;;
;;

(* These functions tick the temporal graph. They produce a pair :
 (new object, function to call on the data graph)
*)

let rec
    tick_scenario s d p o = (Scenario s, graph_ident);

and tick_loop s d p o = (Loop s, graph_ident);

and tick_process newdate newpos offset (p: process) =
    let tick_res = match p.impl with
    | Scenario s -> tick_scenario s newdate newpos offset;
    | Loop l -> tick_loop l newdate newpos offset;
    | None -> (p.impl, graph_ident);
    in ({
        procNode = p.procNode;
        curTime = p.curTime + newdate;
        curOffset = offset;
        curPos = newpos;
        procEnable = p.procEnable;
        impl = tuple_first tick_res
    }, tuple_second tick_res);

and tick_interval itv t offset =
    let new_date = (itv.date + t) in
    let new_pos = (float_of_int new_date /. float_of_int itv.nominalDuration) in
    let tp = tick_process new_date new_pos offset in
    let ticked_procs = (List.map tp itv.processes) in
    ({
      itvId = itv.itvId;
      itvNode = itv.itvNode;
      minDuration = itv.minDuration;
      maxDuration = itv.maxDuration;
      nominalDuration = itv.nominalDuration;
      date = new_date;
      itvStatus = itv.itvStatus;
      processes = (List.map tuple_first ticked_procs)
    },
    (add_tick_to_node itv.itvNode (make_token new_date new_pos offset)) :: (List.map tuple_second ticked_procs))

    
and tick_interval_safe itv t offset = 
    (* tick a given interval without going past the max bound *)
    match itv.maxDuration with 
    | None -> tick_interval itv t offset
    | Some tmax -> tick_interval itv (min t (tmax - itv.date)) offset
    

and start_interval itv t =
    let ticked_procs = (List.map (tick_process 0 0. 0) itv.processes) in
    ({
      itvId = itv.itvId;
      itvNode = itv.itvNode;
      minDuration = itv.minDuration;
      maxDuration = itv.maxDuration;
      nominalDuration = itv.nominalDuration;
      date = 0;
      itvStatus = Waiting;
      processes = (List.map tuple_first ticked_procs)
    },
    (add_tick_to_node itv.itvNode (make_token 0 0. 0)) :: (List.map tuple_second ticked_procs))
and stop_interval itv t = (*todo*)
    itv 

and scenario_event_happen scenario event = 
   (* mark event as executed, add previous intervals to stop set, next intervals to start set *)
   let started_set = 0 in
   let stopped_set = 0 in
   (scenario, event, started_set, stopped_set)

and scenario_event_dispose scenario event = 
   (* mark event as disposed, 
      add previous intervals to stop set, 
      disable next intervals, 
      disable next events if all of their previous intervals are disabled *)
   let stopped_set = 0 in 
   (scenario, event, stopped_set) 
   
(* scenario::process_this
and scenario_process_tc scenario tc statusChangedEvents startedIntervals stoppedIntervals = 
  let itvMinDurReached itv = 
    let prev_ic = find_prev_condition itv in 
    let 
    
  in
  let minDurReached ev = List.map 
*)
;;

(** graph execution functions **)

(* apply a list of functions to the state of the graph *)
let update_graph fun_list graph =
    let apply_rev_fun g f = f g in
    List.fold_left apply_rev_fun graph fun_list ;;

let is_enabled n = (List.length n.tokens) == 0;;

(* todo : disables all nodes which are in strict relationship with a disabled node. *)
let disable_strict_nodes nodes =
    nodes;;

(* todo : topologically sorted list of nodes *)
let topo_sort graph =
    graph.nodes;;

(* todo : when a node can execute *)
let can_execute nodes = true;;

(* todo : remove the data stored in a port *)
let clear_port p =
  p;;

(* todo : copy data from the cables & environment to the port *)
let init_port p graph =
  p;;

(* this sets-up  a node for before its execution *)
let init_node n g e =
 (* clear the outputs of the node *)
 (* and copy data from the environment or edges to the inputs *)
 let init_data = match n.data with
   | Automation (ip, curve) -> Automation (init_port ip g, curve) ;
   | Mapping (ip, op, curve) -> Mapping (init_port ip g, clear_port op, curve) ;
   | Sound (ap, audio) -> Sound (init_port ap g, audio) ;
   | Passthrough (ai, vi, ao, vo) -> Passthrough (init_port ai g, init_port vi g, clear_port ao, clear_port vo) ;
  in { 
    nodeId = n.nodeId;
    data = init_data; 
    executed = n.executed; 
    prev_date = n.prev_date; 
    tokens = n.tokens;
  }
;;

(* actual implementation of the execution algorithm for each kind of process... not really relevant *)
let exec_node_impl data token e =
  data
;;

let exec_node g e { nodeId = id; data = dn; executed = e; prev_date = pd; tokens = tk; } token =
  { nodeId = id;
    data = exec_node_impl dn token e;
    executed = true;
    prev_date = token.tokenDate;
    tokens = tk;
};;

let remove_node l nodeId = List.filter (fun x -> x.nodeId == nodeId) l ;;

(* clear the inputs of a node, and copy its outputs to the environment & delay lines *)
let teardown_node n g e = (n, g, e);;

(* todo *)

(* any input has an edge going in *)
let has_port_input node = true;;

(* any input has an address present in the local scope *)
let has_local_input node = true;;

(* any input has an address present in the global scope *)
let has_global_input node = true;;

(* finds which element occurs at the earliest in a list *)
let rec find_first sorted_nodes n1 n2 =
  match sorted_nodes with
   | [ ] -> raise (Failure "can't happen");
   | h :: t-> if h == n1 then
                 1
               else if h == n2 then
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

let rec sub_tick graph nodes e =
    match nodes with
     | [ ] -> (graph, e);
     | _ ->
        (* look for all the nodes that can be executed at this point *)
        let next_nodes = List.filter can_execute nodes in
        
        (* order them and run the first one *)
        let next_nodes = List.sort (nodes_sort next_nodes) next_nodes in
        match next_nodes with
          | [ ] -> (graph, e) ;
          | cur_node::q ->
            (* set it up by copying data to its inputs *)
            let cur_node = init_node cur_node graph e in
            
            (* execute all the sub-ticks for this node *)
            let cur_node = List.fold_left (exec_node graph e) cur_node cur_node.tokens  in
            
            (* clear its inputs and copy its outputs to the environment or delay lines if relevant *)
            let (cur_node, graph, e) = teardown_node cur_node graph e in
            
            (* repeat on the updated graph *)
            let graph = replace_node graph cur_node.nodeId cur_node in
            sub_tick graph (remove_node next_nodes cur_node.nodeId) e ;;


let tick_graph_topo graph e =
    (* we mark the nodes which had tokens posted to as enabled *)
    let enabled_nodes = disable_strict_nodes (List.filter is_enabled graph.nodes) in
    let sorted_nodes = topo_sort graph in
    let filtered_nodes = List.filter (fun n -> (List.mem n enabled_nodes)) sorted_nodes in
    sub_tick graph filtered_nodes e;;
    
(* Complete example: 2-track sequencer *)
(* 1. Create data graph *)

let test_g = create_graph;;
let (snd_node_1, test_g) = add_node test_g some_sound;;
let (snd_node_2, test_g) = add_node test_g some_sound;;
let (itv_node_1, test_g) = add_node test_g some_passthrough;;
let (itv_node_2, test_g) = add_node test_g some_passthrough;;
let (itv_node_3, test_g) = add_node test_g some_passthrough;;
let (sc_node_1, test_g) = add_node test_g some_passthrough;;
let (itv_node_4, test_g) = add_node test_g some_passthrough;;

test_g;;

(* 2. Create temporal structures *)
let test_itv_1 = {
    itvId = 1;
    itvNode = itv_node_1.nodeId;
    minDuration = 5000;
    maxDuration = Some 5000;
    nominalDuration = 5000;
    date = 0; itvStatus = Waiting;
    processes = [
     {
        procNode = snd_node_1.nodeId;
        procEnable = false;
        curTime = 0;
        curOffset = 0;
        curPos = 0.;
        impl = None;
     }
    ];
};;

let test_itv_2 = {
    itvId = 2;
    itvNode = itv_node_2.nodeId;
    minDuration = 3000;
    maxDuration = Some 3000;
    nominalDuration = 3000;
    date = 0; itvStatus = Waiting;
    processes = [ ];
};;

let test_itv_3 = {
    itvId = 3;
    itvNode = itv_node_3.nodeId;
    minDuration = 5000;
    maxDuration = Some 5000;
    nominalDuration = 5000;
    date = 0; itvStatus = Waiting;
    processes = [
     {
        procNode = snd_node_2.nodeId;
        procEnable = false;
        curTime = 0;
        curOffset = 0;
        curPos = 0.;
        impl = None;
     }
    ];
};;

let test_trig_1 = {
    syncExpr = true_expression;
    conds = [ {
    condExpr = true_expression;
    previousItv = [ ];
    nextItv = [ 1 ];
    status = Waiting;
} ];
};;
let test_trig_2 = {
    syncExpr = true_expression;
    conds = [ {
    condExpr = true_expression;
    previousItv = [ 1 ];
    nextItv = [ 2 ];
    status = Waiting;
} ];
};;
let test_trig_3 = {
    syncExpr = true_expression;
    conds = [ {
    condExpr = true_expression;
    previousItv = [ 2 ];
    nextItv = [ 3 ];
    status = Waiting;
} ];
};;
let test_trig_4 = {
    syncExpr = true_expression;
    conds = [ {
    condExpr = true_expression;
    previousItv = [ 3 ];
    nextItv = [  ];
    status = Waiting;
} ];
};;
let test_scenario = Scenario {
    intervals = [ test_itv_1; test_itv_2; test_itv_3 ];
    triggers = [ test_trig_1; test_trig_2; test_trig_3; test_trig_4 ];
};;

let test_root = {
    itvId = 1; (* we can reuse the id 1 since it's a different hierarchy level *)
    itvNode = itv_node_4.nodeId;
    minDuration = 0;
    maxDuration = None;
    nominalDuration = 10000;
    date = 0;
    itvStatus = Waiting;
    processes = [
     {
        procNode = sc_node_1.nodeId;
        procEnable = false;
        curTime = 0;
        curOffset = 0;
        curPos = 0.;
        impl = test_scenario;
     }
    ]
};;

let env = 0;;
let temporal_tick_res = tick_interval test_root 100 0;;
let test_g = update_graph (tuple_second temporal_tick_res) test_g;;
let (test_g, env2) = tick_graph_topo test_g env;;

let temporal_tick_res = tick_interval (tuple_first temporal_tick_res) 100 0;;
let test_g = update_graph (tuple_second temporal_tick_res) test_g;;
let (test_g, env) = tick_graph_topo test_g env;;

let temporal_tick_res = tick_interval (tuple_first temporal_tick_res) 100 0;;
let test_g = update_graph (tuple_second temporal_tick_res) test_g;;
let (test_g, env) = tick_graph_topo test_g env;;


