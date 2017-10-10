type duration = int;;
type position = float;;
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

type binop = And | Or | Xor;;
type comparator = Greater | GreaterEq | Lower | LowerEq | Equal | Different ;;
type atomElement = AtomParam of parameter | AtomValue of value ;;
type atom = atomElement * atomElement * comparator
and negation = expression
and composition = expression * expression * binop
and impulse = parameter * bool
and expression = Atom of atom | Negation of negation | Composition of composition | Impulse of impulse ;;

let true_expression = Atom ((AtomValue (Bool true)), (AtomValue (Bool true)), Equal);;
let false_expression = Atom ((AtomValue (Bool true)), (AtomValue (Bool true)), Different);;
(* quick checks *)
let test_atom = Atom ((AtomValue (Float 3.4)), (AtomValue (Float 4.5)), Greater);;
let test_comp = Composition (test_atom, test_atom, Or);;

(* the functions we use to work with the expressions *)
let evaluate expr = true ;;
let update expr = expr ;;


type status = Waiting | Pending | Happened | Disposed;;
type message = { messParam: parameter ; messValue: value };;
type state = message list;;

(* ports *)
type edgeType = Glutton | Strict | Delayed ;;
type edge = { edgeId: int; source: int; sink: int; edgeType: edgeType; }
and audioPort = { audioPortId: int; audioPortAddr: audioParameter option; audioEdges: edge list }
and valuePort = { valuePortId: int; valuePortAddr: valueParameter option; valueEdges: edge list }
and port = AudioPort of audioPort | ValuePort of valuePort
;;

(* curves *)
type curve = (float * float) list ;;
let value_at curve x = 0.0;;

(* some processes *)
type automation = valuePort * curve;;
type mapping = valuePort * valuePort * curve;;
type sound = audioPort * float array array;;
type passthrough = audioPort * valuePort * audioPort * valuePort;;

type dataNode = Automation of automation | Mapping of mapping | Sound of sound | Passthrough of passthrough ;;
type grNode = { nodeId: int; data: dataNode; enabled: bool; date: duration; position: position; offset: duration; };;


type graph = { nodes: grNode list ; edges: edge list; };;

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
let test_node_1 = { nodeId = 1; data = some_sound; enabled = false; date = 0; position = 0.; offset = 0; };;
let test_node_2 = { nodeId = 34; data = some_sound; enabled = false; date = 0; position = 0.; offset = 0; };;
next_node_id [ test_node_1; test_node_2 ] ;;

let create_graph = { nodes = []; edges = [] };;
let add_node gr nodeDat =
  let new_id = next_node_id gr.nodes in
  let newNodeDat = match nodeDat with
    | Automation a -> nodeDat
    | Mapping m -> nodeDat
    | Sound s -> nodeDat
    | Passthrough p -> nodeDat
    in
  let new_node = { nodeId = new_id; data = newNodeDat; enabled = false; date = 0; position = 0.; offset = 0; } in
  (new_node, {nodes = new_node::gr.nodes; edges = gr.edges})
;;
let add_edge gr src snk t =
  let new_id = next_edge_id gr.edges in
  let new_edge = { edgeId = new_id; source = src; sink = snk; edgeType = t } in
  (new_edge, { nodes = gr.nodes; edges = new_edge::gr.edges })
;;

(* test *)
let test_g = create_graph;;
let (snd1, test_g) = add_node test_g some_sound;;
let (snd2, test_g) = add_node test_g some_sound;;
let (p1, test_g) = add_node test_g some_passthrough;;

(* let (e1, test_g) = add_edge snd1. *)



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
    previousItv: interval list; (* should not be interval but some reference for instance *)
    nextItv: interval list; (* same. have the structure in scenario instead ? *)
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

let add_process interval proc = match interval with
  | (a,b,c,d,e) -> (a,b,c,d,proc::e);;

(* this supposes that the conditions and triggers are part of the scenario *)
let add_interval sc inv = { intervals = inv::sc.intervals; triggers = sc.triggers };;
let add_trigger sc trg = { intervals = sc.intervals; triggers = trg::sc.triggers };;

let tuple_first (a,b) = a;;
let tuple_second (a,b) = b;;

let find_node graph nodeId = List.find (fun n -> n.nodeId == nodeId) graph.nodes;;
let replace_node graph nodeId newNode =
    {nodes = List.map (fun n -> if n.nodeId == nodeId then newNode else n) graph.nodes;
     edges = graph.edges};;

let graph_ident g = g;;
let graph_tick nodeId enable newdate newpos off graph =
    let node = find_node graph nodeId in
    let new_node = {
        nodeId = node.nodeId;
        data =  node.data;
        enabled = enable;
        date = newdate;
        position = newpos;
        offset = off;
    } in
    replace_node graph nodeId new_node;;
;;

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
    let tp = tick_process (itv.date + t) (float_of_int (itv.date + t) /. float_of_int itv.nominalDuration) offset in
    let ticked_procs = (List.map tp itv.processes) in
    ({
      itvNode = itv.itvNode;
      minDuration = itv.minDuration;
      maxDuration = itv.maxDuration;
      nominalDuration = itv.nominalDuration;
      date = itv.date + t;
      itvStatus = itv.itvStatus;
      processes = (List.map tuple_first ticked_procs)
    },
    List.map tuple_second ticked_procs);;


(** utility functions **)


(* Complete example: 2-track sequencer *)
(* 1. Create graph *)

let test_g = create_graph;;
let (snd_node_1, test_g) = add_node test_g some_sound;;
let (snd_node_2, test_g) = add_node test_g some_sound;;
let (itv_node_1, test_g) = add_node test_g some_passthrough;;
let (itv_node_2, test_g) = add_node test_g some_passthrough;;
let (itv_node_3, test_g) = add_node test_g some_passthrough;;
let (sc_node_1, test_g) = add_node test_g some_passthrough;;
let (itv_node_4, test_g) = add_node test_g some_passthrough;;

test_g;;

let test_itv_1 = {
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
    itvNode = itv_node_2.nodeId;
    minDuration = 3000;
    maxDuration = Some 3000;
    nominalDuration = 3000;
    date = 0; itvStatus = Waiting;
    processes = [ ];
};;

let test_itv_3 = {
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
    nextItv = [ test_itv_1 ];
    status = Waiting;
} ];
};;
let test_trig_2 = {
    syncExpr = true_expression;
    conds = [ {
    condExpr = true_expression;
    previousItv = [ test_itv_1 ];
    nextItv = [ test_itv_2 ];
    status = Waiting;
} ];
};;
let test_trig_3 = {
    syncExpr = true_expression;
    conds = [ {
    condExpr = true_expression;
    previousItv = [ test_itv_2 ];
    nextItv = [ test_itv_3 ];
    status = Waiting;
} ];
};;
let test_trig_4 = {
    syncExpr = true_expression;
    conds = [ {
    condExpr = true_expression;
    previousItv = [ test_itv_3 ];
    nextItv = [  ];
    status = Waiting;
} ];
};;
let test_scenario = Scenario {
    intervals = [ test_itv_1; test_itv_2; test_itv_3 ];
    triggers = [ test_trig_1; test_trig_2; test_trig_3; test_trig_4 ];
};;

let test_root = {
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


tick_interval test_root 100 0;;

(* 2. Create temporal score *)
