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
type grNode = { nodeId: int; data: dataNode; enabled: bool; date: duration; position: position; };;


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
let test_node_1 = { nodeId = 1; data = some_sound; enabled = false; date = 0; position = 0. };;
let test_node_2 = { nodeId = 34; data = some_sound; enabled = false; date = 0; position = 0. };;
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
  let new_node = { nodeId = new_id; data = newNodeDat; enabled = false; date = 0; position = 0. } in
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


type nodeProcess = { 
    node: int; 
    curTime: duration; 
    curOffset: duration;
    curPos: position;    
    };;


type process = 
    NodeProcess of nodeProcess | Scenario of scenario | Loop of loop
and interval = {
    minDuration:duration;
    maxDuration : duration option;
    nominalDuration : duration;
    itvStatus: status;
    processes: process list
}
and condition = { 
    condExpr: expression; 
    previousItv: interval list; 
    nextItv: interval list; 
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

(*let state proc t = proc*)
let add_process interval proc = match interval with
  | (a,b,c,d,e) -> (a,b,c,d,proc::e);;
  
(** utility functions **)
(*
let create_sound_node gr snd = 
   let (node,ng) = create_node gr in 
*)
(* Complete example: 2-track sequencer *)
(* 1. Create graph *)

(*
let my_graph = create_graph;;
let snd = float array array;;
let snd_node_1 = create_sound_node my_graph snd;
let snd_node_2 = create_sound_node my_graph snd;
let itv_node_1 = create_passthrough my_graph;
let itv_node_2 = create_passthrough my_graph;
let sc_node_1 = create_passthrough my_graph;
*)

(* 2. Create temporal score *)
