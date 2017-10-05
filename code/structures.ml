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

let test_atom = Atom ((AtomValue (Float 3.4)), (AtomValue (Float 4.5)), Greater);;
let test_comp = Composition (test_atom, test_atom, Or);;

let evaluate expr = true ;;
let update expr = expr ;;


type status = Waiting | Pending | Happened | Disposed;;
type message = { messParam: parameter ; messValue: value };;
type state = message list;;

(* ports *)
type cable = int ;;
type valuePort = valueParameter * cable list ;;
type audioPort = audioParameter * cable list ;;

(* some processes *)
type curve = (float * float) list ;;
type automation = valuePort * curve;;
type mapping = valuePort * valuePort * curve;;
type sound = audioPort * float array array;;

type process = Automation of automation | Mapping of mapping | Sound of sound;;
type interval = {
    minDuration:int;
    maxDuration : int option;
    nominalDuration : int;
    itvStatus: status;
    processes: process list
};;

type condition = expression * (interval list) * (interval list) * status;;
type trigger = expression * condition list;;

(*let state proc t = proc*)
let add_process interval proc = match interval with
  | (a,b,c,d,e) -> (a,b,c,d,proc::e)
