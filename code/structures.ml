(* utility *)

let tuple_first (a, b) = a;;
let tuple_second (a, b) = b;;

let list_assoc_replace lst key newval = 
  (key, newval) :: (List.remove_assoc key lst)
;;

(* given a function that returns identifiers for elements of a list, 
   return the next biggest identifier *)
let next_id lst f = 
  1 + (List.fold_left max 0 (List.map f lst))
;;

(* base types *)
type duration = int;;
type position = float;;

(* a "None" optional duration means infinite: *)
let is_infinite t = match t with 
  | None -> true ; 
  | _ -> false;;


(*************
 * data tree *
 *************)
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


(***************
 * expressions *
 ***************)
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

type status = Waiting | Pending | Happened | Disposed;;

(**************
 * data model *
 **************)
 
(* ports *)
type edgeType = Glutton | Strict | Delayed ;;
type edge = { edgeId: int; source: int; sink: int; edgeType: edgeType; }
and audioPort = { audioPortId: int; audioPortAddr: audioParameter option; audioEdges: edge list }
and valuePort = { valuePortId: int; valuePortAddr: valueParameter option; valueEdges: edge list }
and port = AudioPort of audioPort | ValuePort of valuePort
;;

type curve = (float * float) list ;;
let value_at curve x = 0.0;;

(* some specific nodes *)
type automation = valuePort * curve;;
type mapping = valuePort * valuePort * curve;;
type sound = audioPort * float array array;;
type passthrough = audioPort * valuePort * audioPort * valuePort;;

(* tokens *)
type token_request = { 
    tokenDate: duration; 
    position: position; 
    offset: duration; 
    start_discontinuous: bool; 
    end_discontinuous: bool; 
};;

let make_token dur pos off = 
{ tokenDate = dur; 
  position = pos; 
  offset = off; 
  start_discontinuous = false; 
  end_discontinuous = false; 
};;

(* nodes of the data graph *)
type dataNode = Automation of automation | Mapping of mapping | Sound of sound | Passthrough of passthrough ;;
type grNode = { nodeId: int; data: dataNode; executed: bool; prev_date: duration; tokens: token_request list; };;

type graph = { nodes: grNode list ; edges: edge list; };;
let create_graph = { nodes = []; edges = [] } ;;


(* utilities and tests *)
let push_token token node = 
  { node with tokens = token::node.tokens }
;;

let next_node_id lst = next_id lst (fun n -> n.nodeId);;
let next_edge_id lst = next_id lst (fun n -> n.edgeId);;

let create_audio_port = { audioPortId = 0; audioPortAddr = None; audioEdges = []; } ;;
let create_value_port = { valuePortId = 0; valuePortAddr = None; valueEdges = []; } ;;

(* test *)
let test_edge = { edgeId = 33; source = 4; sink = 5; edgeType = Glutton; };;
let some_sound_data = Array.make 2 (Array.make 8 0.);;
let some_sound = Sound (create_audio_port, some_sound_data);;

let some_passthrough = Passthrough ( create_audio_port, create_value_port, create_audio_port, create_value_port );;

(* test *)
let test_node_1 = { nodeId = 1; data = some_sound; executed = false; prev_date = 0; tokens = [ ]; } ;;
let test_node_2 = { nodeId = 34; data = some_sound; executed = false; prev_date = 0; tokens = [ ]; } ;;
next_node_id [ test_node_1; test_node_2 ] ;;

(* get a new port id for when adding nodes to the graph *)
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

(* add a node to the data graph *)
let add_node gr nodeDat =
  let update_vp_id vp new_id = { vp with valuePortId = new_id } in
  let update_ap_id ap new_id = { ap with audioPortId = new_id } in 

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
  (new_node, { gr with nodes = new_node::gr.nodes; })
;;

(* add an edge between two ports of the data graph *)
let add_edge gr src snk t =
  let new_id = next_edge_id gr.edges in
  let new_edge = { edgeId = new_id; source = src; sink = snk; edgeType = t } in
  (new_edge, { gr with edges = new_edge::gr.edges })
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


(******************************
 * temporal model definitions *
 ******************************)
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
    syncId: int;
    syncExpr: expression;
    conds: condition list
}
and scenario = {
    intervals: interval list ;
    triggers: temporalCondition list;
    root_triggers: int list;
}
and loop = {
    pattern: interval;
    startTrig: temporalCondition;
    endTrig: temporalCondition;
};;

(* utility functions to work with scenario *)
let find_prev_condition itv scenario = 
  let conditions = List.concat (List.map (fun t -> t.conds) scenario.triggers) in 
  List.find (fun ic -> (List.exists (fun id -> id == itv.itvId) ic.nextItv)) conditions
;;

let find_next_condition itv scenario = 
  let conditions = List.concat (List.map (fun t -> t.conds) scenario.triggers) in 
  List.find (fun ic -> (List.exists (fun id -> id == itv.itvId) ic.previousItv)) conditions
;;

let find_parent_trig cond scenario =
  List.find (fun x -> (List.mem cond x.conds)) scenario.triggers
;;
  
let find_end_trig itv scenario = 
  find_parent_trig (find_next_condition itv scenario) scenario
;;
  
let add_process interval proc = 
  { interval with processes = proc::interval.processes }
;;

(* this supposes that the conditions and triggers are part of the scenario *)

let add_interval sc inv = 
  let next_interval_id lst = next_id lst (fun n -> n.itvId) in
  { sc with 
    intervals = { inv with itvId = next_interval_id sc.intervals }::sc.intervals;
};;

let add_trigger sc trg = 
  let next_trigger_id lst = next_id lst (fun n -> n.syncId) in
  { sc with 
    triggers = { trg with syncId = next_trigger_id sc.triggers }::sc.triggers 
};;

let find_node graph nodeId = List.find (fun n -> n.nodeId == nodeId) graph.nodes;;
let replace_node graph nodeId newNode =
  { graph with 
    nodes = List.map (fun n -> if n.nodeId == nodeId then newNode else n) graph.nodes; 
};;

let graph_ident g = g;;

(* register a tick to a node in the graph and return the new graph *)
let add_tick_to_node nodeId token graph =
    (* tokens go from oldest to newest *)
    let node = (find_node graph nodeId) in
    let new_node = { 
        node with 
        tokens = node.tokens @ [ token ]; 
    } in
    replace_node graph nodeId new_node;;
;;

(* These functions tick the temporal graph. They produce a pair :
 (new object, function to call on the data graph)
*)
let rec tick_loop s d p o = 
  (Loop s, graph_ident);

(* actions & triggerings might happen on start / stop *)
(* starting of processes *)

(* when a scenario starts, we look up all the "root" temporal conditions : 
   they are not preceded by any interval and start on the condition "true" *)
and start_scenario s = 
  let is_root trigger = 
    (trigger.syncExpr == true_expression)
    && 
    (List.for_all (fun c -> c.previousItv == [ ]) trigger.conds)
  in
  
  Scenario { 
    s with root_triggers = 
        (List.map (fun x -> x.syncId) (List.filter is_root s.triggers))
   };

and start_loop l = 
   Loop l;
  
(* 
For processes, add a first tick at t=0 when starting them.
No samples will be produced so the offset does not matter. 
*)
and start_process p =
    let res = match p.impl with
    | Scenario s -> start_scenario s;
    | Loop l -> start_loop l;
    | None -> p.impl;
    in (
    { p with 
          curTime = 0;
          curOffset = 0;
          curPos = 0.;
          impl = res
    }, 
    (add_tick_to_node p.procNode (make_token 0 0. 0)) 
);

(* stopping of processes *)
and stop_scenario s = 
  (Scenario s, graph_ident);

and stop_loop l = 
  (Loop l, graph_ident);
  
and stop_process p =
    let res = match p.impl with
    | Scenario s -> stop_scenario s;
    | Loop l -> stop_loop l;
    | None -> (p.impl, graph_ident);
    in ({ p with impl = tuple_first res}, tuple_second res);

(* ticking of processes: increase the time. *)
and tick_process newdate newpos offset (p: process) =
    let tick_res = match p.impl with
    | Scenario s -> tick_scenario s newdate newpos offset;
    | Loop l -> tick_loop l newdate newpos offset;
    | None -> (p.impl, graph_ident);
    in ({ p with 
          curTime = p.curTime + newdate;
          curOffset = offset;
          curPos = newpos;
          impl = tuple_first tick_res
    }, tuple_second tick_res);

(* ticking of intervals: aggregate all the ticks of the processes *)
and tick_interval itv t offset =
    let new_date = (itv.date + t) in
    let new_pos = (float_of_int new_date /. float_of_int itv.nominalDuration) in
    let tp = tick_process new_date new_pos offset in
    let ticked_procs = (List.map tp itv.processes) in
    ({ itv with
       date = new_date;
       processes = (List.map tuple_first ticked_procs)
    },
    (List.map tuple_second ticked_procs) @  [ add_tick_to_node itv.itvNode (make_token new_date new_pos offset) ] )


and start_interval itv t =
    let ticked_procs = (List.map start_process itv.processes) in
    ({ itv with
       date = 0;
       itvStatus = Waiting;
       processes = (List.map tuple_first ticked_procs)
    },
    (List.map tuple_second ticked_procs) @ [ add_tick_to_node itv.itvNode (make_token 0 0. 0) ] )
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
    
(* this functions ticks an interval in the context of a scenario *)
and scenario_run_interval scenario interval overticks tick offset = 
  let itv_old_date = interval.date in 
  let end_node = find_end_trig interval scenario  in
  match interval.maxDuration with 
  (* if there is no max, we can go to the whole length of the tick *)
  | None -> (tick_interval interval tick offset, overticks)
  (* if there is a max, we have to stop at the max and save the remainings *)
  | Some maxdur -> 
    let actual_tick = min tick (maxdur - interval.date) in
    let tick_res = tick_interval interval actual_tick offset in 
    let overtick = tick - (maxdur - interval.date) in 
    (* find if there was already over-ticks recorded for this trigger, and if so, update them *)
    match List.assoc_opt end_node.syncId overticks with 
    | None -> (tick_res, (end_node.syncId, (overtick, overtick))::overticks)
    | Some (min_ot, max_ot) ->
            let new_overtick = (min overtick min_ot, max overtick max_ot) in 
            (tick_res, list_assoc_replace overticks end_node.syncId new_overtick)

and scenario_process_trigger scenario tc statusChangedEvents startedIntervals stoppedIntervals = 
  tc(* 
  let running_itvs = List.find (fun x -> x.date != 0) scenario.intervals in
  let itvMinDurReached itv = 
    let prev_ic = find_prev_condition itv in 
    prev_ic
    
  in
  let minDurReached ev = List.map  *)
  
and tick_scenario scenario dur pos offset =
  let is_interval_running itv = 
    itv.date != 0 && 
    (find_next_condition itv scenario).status != Happened in
    
  let running_itvs = List.filter is_interval_running scenario.intervals in
  
  (Scenario scenario, graph_ident);
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
  in { n with data = init_data; }
;;

(* actual implementation of the execution algorithm for each kind of process... not really relevant *)
let exec_node_impl data token =
  data
;;

let exec_node g n token =
  { n with 
    data = exec_node_impl n.data token;
    executed = true;
    prev_date = token.tokenDate;
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
            (* note: actually it's ok to reuse the same environment: it is only used for reading *)
            let cur_node = List.fold_left (exec_node graph) cur_node cur_node.tokens  in
            
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
    syncId = 1;
    syncExpr = true_expression;
    conds = [ {
    condExpr = true_expression;
    previousItv = [ ];
    nextItv = [ 1 ];
    status = Waiting;
} ];
};;
let test_trig_2 = {
    syncId = 2;
    syncExpr = true_expression;
    conds = [ {
    condExpr = true_expression;
    previousItv = [ 1 ];
    nextItv = [ 2 ];
    status = Waiting;
} ];
};;
let test_trig_3 = {
    syncId = 3;
    syncExpr = true_expression;
    conds = [ {
    condExpr = true_expression;
    previousItv = [ 2 ];
    nextItv = [ 3 ];
    status = Waiting;
} ];
};;
let test_trig_4 = {
    syncId = 4;
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
  root_triggers = [ test_trig_1.syncId ];  
}
;;

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
(*
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
