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

let lot_tol_test = lot_to_tol3 [ (1, 2, 3) ; (4, 5, 6) ] ;;
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
  local: string -> value option;
  global: string -> value
};;
let empty_env = {
  local = noenv_opt;
  global = noenv
};;

let pull var e =
  match (e.local var) with
  | None -> e.global var
  | Some v -> v
;;
let push var va e =
  { e with
    local = fun v -> if v = var then va else (e.local var)
};;
(** in the C++ code this function applies
    the data recorded in the local environment to the global environment,
    write buffers to the sound card, etc etc *)
let commit e = { e with local = noenv_opt };;



(***************
 * expressions *
 ***************)

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
  | Impulse   of string*bool
;;

exception EvalError;;

let rec evaluate expr (e:environment) =
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
  | Negation e1       -> not (evaluate e1 e)
  | And (e1,e2)       -> (evaluate e1 e) && (evaluate e2 e)
  | Or (e1,e2)        -> (evaluate e1 e) || (evaluate e2 e)
  | Impulse (v,b)     -> b (* instead, check if the env contains the value ? *)


(* two useful expressions *)
let true_expression = Equal (Value (Bool true), Value (Bool true));;
let false_expression = Equal (Value (Bool true), Value (Bool false));;

let update expr = expr ;;

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
    portValue: value option
};;

type curve = float -> float;;

(* some specific nodes *)
type automation = port * curve;;
type mapping = port * port * curve;;
type sound = port * float array array;;
type passthrough = port * port;;

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
type dataNode =
    Automation of automation
  | Mapping of mapping
  | Sound of sound
  | Passthrough of passthrough;;
type grNode = {
    nodeId: nodeId;
    data: dataNode;
    executed: bool;
    prev_date: duration;
    tokens: token_request list;
};;

type graph = {
    nodes: grNode list;
    edges: edge list;
};;

(* utilities and tests *)
let create_graph = { nodes = []; edges = [] } ;;

let push_token token node =
  { node with tokens = (node.tokens@[token]); }
;;

let next_node_id lst = NodeId (next_id lst (fun n -> let (NodeId id) = n.nodeId in id));;
let next_edge_id lst = EdgeId (next_id lst (fun n -> let (EdgeId id) = n.edgeId in id));;

let create_port = { portId = PortId 0; portAddr = None; portEdges = []; portValue = None; } ;;

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
  let new_node = { nodeId = new_id; data = newNodeDat; executed = false; prev_date = 0; tokens = [ ]; } in
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
let find_edge graph edgeId =
  List.find (fun n -> n.edgeId = edgeId) graph.edges
;;

(* find a port in a graph by id *)
exception PortNotFound;;

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




(******************************
 * temporal model definitions *
 ******************************)
type intervalId = IntervalId of int;;
type tempCondId = TempCondId of int;;

type processImpl =
    Scenario of scenario | Loop of loop | DefaultProcess
and process = {
  procNode: nodeId;
  procEnable: bool;
  curTime: duration;
  impl: processImpl;
}
and interval = {
  itvId: intervalId;
  itvNode: nodeId;
  minDuration: duration;
  maxDuration : duration option;
  nominalDuration : duration;
  date : duration;
  speed: float;
  processes: process list
}
and condition = {
  parentSync: tempCondId;
  condExpr: expression;
  previousItv: intervalId list;
  nextItv: intervalId list;
  status: status;
}
and temporalCondition = {
  tcId: tempCondId;
  syncExpr: expression;
  conds: condition list
}
and scenario = {
  intervals: interval list ;
  tempConds: temporalCondition list;
  root_tempConds: tempCondId list;
}
and loop = {
  pattern: interval;
  startTC: temporalCondition;
  endTC: temporalCondition;
};;

(* utility functions to work with scenario *)
let find_prev_IC itv scenario =
  let conditions = List.concat (List.map (fun t -> t.conds) scenario.tempConds) in
  List.find (fun ic ->
      (List.exists (fun id -> id = itv.itvId) ic.nextItv)
    )
    conditions
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

let add_process interval proc =
  { interval with processes = proc::interval.processes }
;;

let replace_TC scenario tc =
  { scenario with
    tempConds = List.map (fun x -> if x.tcId = tc.tcId then tc else x) scenario.tempConds;
  }
;;

let replace_interval scenario itv =
  { scenario with
    intervals = List.map (fun x -> if x.itvId = itv.itvId then itv else x) scenario.intervals;
  }
;;

let get_intervals id_list scenario =
  List.filter (fun x -> List.mem x.itvId id_list) scenario.intervals;;
let get_temporalConds id_list scenario =
  List.filter (fun x -> List.mem x.tcId id_list) scenario.tempConds;;

let replace_intervals scenario itvs =
  List.fold_left replace_interval scenario itvs
;;

let is_interval_running scenario itv =
  (find_prev_IC itv scenario).status = Happened &&
  (find_next_IC itv scenario).status <> Happened;;

let update_conds scenario tc iclist =
  let new_tc = { tc with conds = iclist } in
  replace_TC scenario new_tc
;;



(*****************************
 * temporal model algorithms *
 *****************************)

(* These functions tick the temporal graph. They produce a pair :
   (new object, function to call on the data graph)
*)
let rec tick_loop s d p o e =
  (s, graph_ident);

  (* actions & triggerings might happen on start / stop *)
  (* starting of processes *)

  (* when a scenario starts, we look up all the "root" temporal conditions :
     they are not preceded by any interval and start on the condition "true" *)
and start_scenario s =
  let is_root tc =
    (tc.syncExpr = true_expression)
    &&
    (List.for_all (fun c -> c.previousItv = [ ]) tc.conds)
  in
  Scenario {
    s with
    root_tempConds =
             (List.map (fun x -> x.tcId) (List.filter is_root s.tempConds))
  };

and start_loop l =
  Loop l;

(*
For processes, add a first tick at t=0 when starting them.
No samples will be produced so the offset does not matter.
*)
and start_process p =
    ({ p with
      curTime = 0;
      impl = match p.impl with
             | Scenario s -> start_scenario s;
             | Loop l -> start_loop l;
             | DefaultProcess -> p.impl
    },
    add_tick_to_node p.procNode (make_token 0 0. 0)
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
    | DefaultProcess -> (p.impl, graph_ident);
  in ({ p with impl = tuple_first res}, tuple_second res);

  (* ticking of processes: increase the time. *)
and tick_process newdate newpos offset (e:environment) p  =
  let tick_res = match p.impl with
    | Scenario s -> let (p, f) = tick_scenario s p.curTime newdate newpos offset e
                    in (Scenario p, f);
    | Loop l -> let (p, f) = tick_loop l newdate newpos offset e
                in (Loop p, f);
    | DefaultProcess -> (p.impl, add_tick_to_node p.procNode (make_token newdate newpos offset));
  in ({ p with
        curTime = newdate;
        impl = tuple_first tick_res
      }, tuple_second tick_res);

  (* ticking of intervals: aggregate all the ticks of the processes *)
and tick_interval itv t offset (e:environment) =
  let new_date = (itv.date + (truncate (ceil (float t) *. itv.speed))) in
  let new_pos = (float new_date /. float itv.nominalDuration) in
  let tp = tick_process new_date new_pos offset e in
  let ticked_procs = (List.map tp itv.processes) in
  ({ itv with
     date = new_date;
     processes = (List.map tuple_first ticked_procs)
   },
   (List.map tuple_second ticked_procs) @  [ add_tick_to_node itv.itvNode (make_token new_date new_pos offset) ] )


and start_interval itv =
  let ticked_procs = (List.map start_process itv.processes) in
  ({ itv with
     date = 0;
     processes = (List.map tuple_first ticked_procs)
   },
    ((List.map tuple_second ticked_procs) @ [ add_tick_to_node itv.itvNode (make_token 0 0. 0) ]) )

and stop_interval itv = (*todo*)
  itv

and scenario_ic_happen scenario ic =
  (* mark ic as executed, add previous intervals to stop set, next intervals to start set *)
  let started_set = ic.nextItv in
  let stopped_set = ic.previousItv in
  ({ ic with status = Happened }, started_set, stopped_set)

and scenario_ic_dispose scenario ic =
  (* mark ic as disposed,
     add previous intervals to stop set,
     disable next intervals,
     disable next ics if all of their previous intervals are disabled *)
  let stopped_set = ic.previousItv in
  ({ ic with status = Disposed }, [ ], stopped_set)

(* this functions ticks an interval in the context of a scenario.
   it returns ( (new_interval, list of functions to apply), overticks )
 *)
and scenario_run_interval scenario overticks tick offset interval e =
  let end_TC = find_end_TC interval scenario in

  match interval.maxDuration with
    (* if there is no max, we can go to the whole length of the tick *)
    | None -> (tick_interval interval tick offset e, overticks)

    (* if there is a max, we have to stop at the max and save the remainings *)
    | Some maxdur ->
      let actual_tick = min tick (maxdur - interval.date) in
      let tick_res = tick_interval interval actual_tick offset e in
      let overtick = tick - (maxdur - interval.date) in

      (* find if there was already over-ticks recorded for this TC, and if so, update them *)
      match List.assoc_opt end_TC.tcId overticks with
        | None -> (tick_res, (end_TC.tcId, (overtick, overtick))::overticks)
        | Some (min_ot, max_ot) ->
            let new_overtick = (min overtick min_ot, max overtick max_ot) in
            (tick_res, list_assoc_replace overticks end_TC.tcId new_overtick)

(* this function does the evaluation & execution of a given temporal condition *)
and scenario_process_TC scenario tc (e:environment) =

  (**** utilities ****)

  (* minDurReached ic = true iff all the non-disposed previous intervals
     have reached their min duration *)
  let minDurReached ic =
    (* find the intervals in the evaluation area *)
    let min_reached itv =
      (itv.date >= itv.minDuration) ||
      (find_prev_IC itv scenario).status = Disposed
    in
    List.for_all min_reached (get_intervals ic.previousItv scenario)
  in

  (* maxDurReached ic = true iff any of the previous intervals
     have reached their max duration *)
  let maxDurReached ic =
    let max_reached itv =
       match itv.maxDuration with
       | None -> false
       | Some t -> itv.date >= t
    in
    List.exists max_reached (get_intervals ic.previousItv scenario)
  in

  (* execution of a given instantaneous condition *)
  (* returns (ic, started intervals, stopped intervals *)
  let execute_ic scenario e ic =
    let ic = { ic with condExpr = update ic.condExpr } in
    if evaluate ic.condExpr e
    then
      scenario_ic_happen scenario ic
    else
      scenario_ic_dispose scenario ic
  in

  (* execution of a given temporal condition *)
  (* returns (new_scenario, [ instantaneous conditions that executed ], [ functions to apply to the data graph ]) *)
  let execute_tc scenario tc =
    (* execute the conditions *)
    let (new_conds, started_itv_ids, ended_itv_ids) =
        lot_to_tol3 (List.map (execute_ic scenario e) tc.conds) in

    (* start and stop the intervals *)
    let ended_intervals =
        List.map
            stop_interval
            (get_intervals (List.flatten ended_itv_ids) scenario) in
    let (started_intervals, funs) = lot_to_tol2
        (List.map
            start_interval
            (get_intervals (List.flatten started_itv_ids) scenario)) in

    let funs = List.flatten funs in
    (* replace the conditions by the new ones, same for the intervals.
       besides, we also keep the functions generated by the start of intervals *)
    (replace_intervals
        (replace_TC scenario { tc with conds = new_conds })
        (ended_intervals@started_intervals)
    , new_conds
    , funs)
  in

  (**** actual execution ****)

  (* mark all instantaneous conditions with min reached as Pending *)
  let updConds =
    List.map
      (fun x -> if minDurReached x then { x with status = Pending } else x)
      tc.conds
  in

  (* amongst all the pending ones, we check if any has reached its max *)
  let tcMaxDurReached =
    List.exists
      (fun ic -> ic.status = Pending && maxDurReached ic)
      updConds
  in

  (* replace them in the scenario *)
  let scenario = (update_conds scenario tc updConds) in

  (* if not all ICs are pending or disposed *)
  if (not (List.for_all (fun x -> x.status = Pending || x.status = Disposed) updConds))
  then
    (scenario, [ ], [ ])
  else
    if ((tc.syncExpr <> true_expression) && (not tcMaxDurReached))
    then
      let tc = { tc with syncExpr = update tc.syncExpr } in

      if (not (evaluate tc.syncExpr e))
      then
        (* expression is false, do nothing apart updating the TC *)
        (replace_TC scenario tc, [ ], [ ])
      else
        (* the tc expression is true, we can proceed with the execution of what follows *)
        execute_tc scenario tc
    else
     (* max reached or true expression, we can execute the temporal condition  *)
     execute_tc scenario tc

and tick_scenario scenario olddate newdate pos offset (e:environment) =
  let dur = newdate - olddate in
  (* execute the list of root TCs.
     l1 : list of executed ICs
     l2 : list of resulting functions  *)
  let rec process_root_tempConds scenario tc_list (l1, l2) =
    match tc_list with
    | [ ] -> (scenario, l1, l2)
    | h::t ->
        (* try to execute the TC *)
        let (scenario, conds, funs) =
            scenario_process_TC scenario h e in

        if (List.length conds) = 0 then
          (* No new IC, the trigger wasn't executed, we keep it *)
          (* note: the user interface enforces that a TC always has at least a single IC *)
          process_root_tempConds scenario t ((conds @ l1), (funs @ l2))
        else
          (* the TC was executed, remove it from the roots *)
          process_root_tempConds
            { scenario with root_tempConds = List.filter (fun x -> x <> h.tcId) scenario.root_tempConds }
            t
            ((conds @ l1), (funs @ l2))
  in

  (* execute a given list of TCs *)
  let rec process_tempConds scenario tc_list (l1, l2) =
    match tc_list with
    | [ ] -> (scenario, l1, l2)
    | h::t ->
        (* try to execute the TC *)
        let (scenario, conds, funs) =
            scenario_process_TC scenario h e in
        process_tempConds scenario t ((conds @ l1), (funs @ l2))
  in

  (* execute a list of intervals *)
  let rec process_intervals scenario itv_list overticks funs dur offset end_TCs =
    match itv_list with
    | [ ] -> (scenario, overticks, end_TCs, funs)
    | interval::t ->
        (* run the interval and replace it in a new scenario *)
        let ((new_itv, new_funs), overticks) =
            scenario_run_interval scenario overticks dur offset interval e in
        process_intervals
         (replace_interval scenario new_itv)
         t overticks
         (funs@new_funs)
         dur offset
         ((find_end_TC new_itv scenario)::end_TCs)
  in

  let rec finish_tick scenario overticks conds funcs dur offset end_TCs =
    match conds with
    | [ ] ->
      (* now we can process remaining end_TCs *)
      (match end_TCs with
       (* nothing to execute anymore *)
       | [ ] -> (scenario, funcs)
       (* some TCs reached their end so we execute them *)
       | _ -> let (scenario, conds, cond_funcs) =
                process_tempConds scenario end_TCs ([], funcs) in
             finish_tick scenario overticks conds cond_funcs dur offset [ ])

    | (cond:condition) :: remaining ->
      (* look if an over-tick was recorded for the TC *)
      match (List.assoc_opt (find_parent_TC cond scenario).tcId overticks) with
      | None -> finish_tick scenario overticks remaining funcs dur offset end_TCs
      | Some (min_t, max_t) ->
         (* we can go forward with executing some intervals *)
         let (scenario, overticks, end_TCs, funcs) =
             process_intervals
                scenario
                (following_intervals cond scenario)
                overticks funcs
                max_t
                (offset + dur - max_t)
                end_TCs
         in
         finish_tick scenario overticks remaining funcs dur offset end_TCs
  in

  (*** actual execution begins here ***)

  (* first execute the root temporal conditions, if any *)
  let (scenario, conds, funcs) =
    process_root_tempConds
        scenario
        (get_temporalConds scenario.root_tempConds scenario)
        ([], []) in

  (* run the intervals that follows them *)
  let (scenario, overticks, end_TCs, funcs) =
    process_intervals scenario (List.filter (is_interval_running scenario) scenario.intervals ) [] funcs dur offset [] in

  (* run potential terminating temporal conditions *)
  let (scenario, conds, funcs) = process_tempConds scenario end_TCs ([], funcs) in

  (* loop until the time cannot be advanced in any branch anymore *)
  let (scenario, funcs) = finish_tick scenario overticks conds funcs dur offset end_TCs in
  (scenario, list_fun_combine funcs)
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
let disable_strict_nodes nodes =
  nodes;;

(* todo : topologically sorted list of nodes *)
let topo_sort graph =
  graph.nodes;;

(* todo : when a node can execute *)
let can_execute nodes = true;;

(* remove the data stored in a port *)
let clear_port p = { p with portValue = None };;

let get_edges edges gr =
  List.find_all (fun x -> List.mem x.edgeId edges) gr.edges;;

(* some combination function *)
let combine (v1:value) (v2:value) =
  match (v1, v2) with
  | (Audio a1, Audio a2) -> Audio a1 (* mix audio *)
  | (_,v) -> v;; (* take latest *)

(* reads the data of the source when there is a cable between two ports *)
let get_src_value edge gr =
  let src_port = find_port gr edge.source in
  match edge.edgeType with
  | Strict -> src_port.portValue
  | Glutton -> src_port.portValue
  (* these have their own buffer line where data is copied *)
  | DelayedStrict dl -> raise Todo;
  | DelayedGlutton dl -> raise Todo;
;;

(* combine read data with existing data in a port *)
let aggregate_data gr (v: value option) edge  =
  match (v, get_src_value edge gr) with
  | (None, None) -> None
  | (None, v) -> v
  | (v, None) -> v
  | (Some v1, Some v2) -> Some (combine v1 v2)
;;

(* copy data from the cables & environment to the port *)
let init_port (p:port) g (e:environment) =
  match p.portEdges with
  (* no edges: read from the env *)
  | [] -> let pv = match p.portAddr with
                | None -> None
                | Some str -> Some (pull str e)
                in
         { p with portValue = pv }
  (* edges: read from them *)
  | _ -> { p with portValue = (
                List.fold_left (aggregate_data g) None (get_edges p.portEdges g) )
         }
  ;;

(* this sets-up a node for before its execution *)
let init_node n g (e:environment) =
  (* clear the outputs of the node *)
  (* and copy data from the environment or edges to the inputs *)
  { n with data =
        match n.data with
        | Automation (op, curve) -> Automation (clear_port op, curve) ;
        | Mapping (ip, op, curve) -> Mapping (init_port ip g e, clear_port op, curve) ;
        | Sound (op, audio) -> Sound (clear_port op, audio) ;
        | Passthrough (ip, op) -> Passthrough (init_port ip g e, clear_port op);
};;

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

let in_port_disabled edge graph =
  not (is_enabled (find_port_node graph (find_edge graph edge).sink));;

let write_port_env p e =
  match p.portAddr with
  | Some addr -> (push addr p.portValue e)
  | None -> e
;;
let write_port_edges p g =
  g (*todo*)
;;

(* write the data in a port to the cables or environment if available *)
let write_port p g e =
  let has_targets = (p.portEdges = []) in
  let all_targets_disabled =
    has_targets &&
    List.for_all (fun x -> in_port_disabled x g) p.portEdges in
  if(not has_targets || all_targets_disabled) then
    (g, write_port_env p e)
  else
    (write_port_edges p g, e)
;;

(* clear the inputs of a node, and copy its outputs to the environment & delay lines *)
let teardown_node n g e =
  let (g_res, data_res, e_res) =
  match n.data with
  | Automation (op, curve) -> let (g, e) = write_port op g e in (g, Automation (op, curve), e);
  | Sound (op, snd) -> let (g, e) = write_port op g e in (g, Sound (op, snd), e);
  | Mapping (ip, op, curve) -> let (g, e) = write_port op g e in (g, Mapping (clear_port ip, op, curve), e);
  | Passthrough (ip, op) -> let (g, e) = write_port op g e in (g, Passthrough (clear_port ip, op), e);
  in
  ({ n with data = data_res}, g_res, e_res);;

(* todo *)

(* any input has an edge going in *)
let has_port_input node = true;;

(* any input has an address present in the local scope *)
let has_local_input node = true;;

(* any input has an address present in the global scope *)
let has_global_input node = true;;

(* remove all the tokens of all the nodes *)
let clear_tokens graph =
  let rec impl before after =
    match before with
    | [ ] -> { graph with nodes = after }
    | h :: t -> impl t ({h with tokens = [ ]} :: after)
  in
    impl graph.nodes [ ]
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

let rec sub_tick graph nodes (e:environment) =
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
      let cur_node = List.fold_left (exec_node graph) cur_node cur_node.tokens in

      (* clear its inputs and copy its outputs to the environment or delay lines if relevant *)
      let (cur_node, graph, e) = teardown_node cur_node graph e in

      (* repeat on the updated graph, with the remaining nodes *)
      sub_tick
        (replace_node graph cur_node.nodeId cur_node)
        (remove_node next_nodes cur_node.nodeId)
        e ;;


let tick_graph_topo graph e =
  (* we mark the nodes which had tokens posted to as enabled *)
  let enabled_nodes = disable_strict_nodes (List.filter is_enabled graph.nodes) in
  let sorted_nodes = topo_sort graph in
  let filtered_nodes = List.filter (fun n -> (List.mem n enabled_nodes)) sorted_nodes in
  let (graph, e) = sub_tick graph filtered_nodes e in
  (clear_tokens graph, e);;


(** this simulates the arrival of new data in the environment :
    audio inputs, etc. **)
let update e = e;;

(** overall main loop: run a score for some amount of time,
    at a given granularity (eg tick every 50 units) **)
let rec main_loop root graph duration granularity (e:environment) =
  if duration > 0
  then
    let (root, funs) = tick_interval root granularity 0 e in
    let (graph, e)   = tick_graph_topo (update_graph funs graph) e in
    main_loop root graph (duration - granularity) granularity (update (commit e))
  else
    (root, graph, e)
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
let test_node_1 = { nodeId = NodeId 1; data = some_sound; executed = false; prev_date = 0; tokens = [ ]; } ;;
let test_node_2 = { nodeId = NodeId 34; data = some_sound; executed = false; prev_date = 0; tokens = [ ]; } ;;
next_node_id [ test_node_1; test_node_2 ] ;;

(* quick checks *)
let test_edge = { edgeId = EdgeId 33; source = PortId 4; sink = PortId 5; edgeType = Glutton; };;
let some_sound_data = Array.make 2 (Array.make 8 0.);;
let some_sound = Sound (create_port, some_sound_data);;

let some_passthrough = Passthrough ( create_port, create_port);;

(* quick checks *)
let test_node_1 = { nodeId = NodeId 1; data = some_sound; executed = false; prev_date = 0; tokens = [ ]; };;
let test_node_2 = { nodeId = NodeId 34; data = some_sound; executed = false; prev_date = 0; tokens = [ ]; };;
next_node_id [ test_node_1; test_node_2 ] ;;

(* quick checks *)
let test_g = create_graph;;
let (snd1, test_g) = add_node test_g some_sound;;
let (snd2, test_g) = add_node test_g some_sound;;
let (p1, test_g) = add_node test_g some_passthrough;;


let nodes = [
    { nodeId = NodeId 1; data = some_sound; executed = false; prev_date = 0; tokens = [ ]};
    { nodeId = NodeId 2; data = some_sound; executed = false; prev_date = 0; tokens = [ ] }
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
  minDuration = 5000;
  maxDuration = Some 5000;
  nominalDuration = 5000;
  date = 0;
  speed = 1.;
  processes = [
    {
      procNode = snd_node_1.nodeId;
      procEnable = false;
      curTime = 0;
      impl = DefaultProcess;
    }
  ];
} in

let test_itv_2 = {
  itvId = IntervalId 2;
  itvNode = itv_node_2.nodeId;
  minDuration = 3000;
  maxDuration = Some 3000;
  nominalDuration = 3000;
  date = 0;
  speed = 1.;
  processes = [ ];
} in

let test_itv_3 = {
  itvId = IntervalId 3;
  itvNode = itv_node_3.nodeId;
  minDuration = 5000;
  maxDuration = Some 5000;
  nominalDuration = 5000;
  date = 0;
  speed = 1.;
  processes = [
    {
      procNode = snd_node_2.nodeId;
      procEnable = false;
      curTime = 0;
      impl = DefaultProcess;
    }
  ];
} in


let test_TC_1 = {
  tcId = TempCondId 1;
  syncExpr = true_expression;
  conds = [ {
      parentSync = TempCondId 1;
      condExpr = true_expression;
      previousItv = [ ];
      nextItv = [ IntervalId 1 ];
      status = Waiting;
    } ];
} in

let test_TC_2 = {
  tcId = TempCondId 2;
  syncExpr = true_expression;
  conds = [ {
      parentSync = TempCondId 2;
      condExpr = true_expression;
      previousItv = [ IntervalId 1 ];
      nextItv = [ IntervalId 2 ];
      status = Waiting;
    } ];
} in

let test_TC_3 = {
  tcId = TempCondId 3;
  syncExpr = true_expression;
  conds = [ {
      parentSync = TempCondId 3;
      condExpr = true_expression;
      previousItv = [ IntervalId 2 ];
      nextItv = [ IntervalId 3 ];
      status = Waiting;
    } ];
} in

let test_TC_4 = {
  tcId = TempCondId 4;
  syncExpr = true_expression;
  conds = [ {
      parentSync = TempCondId 4;
      condExpr = true_expression;
      previousItv = [ IntervalId 3 ];
      nextItv = [  ];
      status = Waiting;
    } ];
} in


let test_scenario = Scenario {
    intervals = [ test_itv_1; test_itv_2; test_itv_3 ];
    tempConds = [ test_TC_1; test_TC_2; test_TC_3; test_TC_4 ];
    root_tempConds = [ test_TC_1.tcId ];
  }
in

let test_root = {
  itvId = IntervalId 1; (* we can reuse the id 1 since it's a different hierarchy level *)
  itvNode = itv_node_4.nodeId;
  minDuration = 0;
  maxDuration = None;
  nominalDuration = 10000;
  date = 0;
  speed = 1.;
  processes = [
    {
      procNode = sc_node_1.nodeId;
      procEnable = false;
      curTime = 0;
      impl = test_scenario;
    }
  ]
} in
 main_loop test_root test_g 7000 1000 empty_env;;

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
