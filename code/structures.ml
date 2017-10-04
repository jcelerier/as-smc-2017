type value = Float of float | Int of int | Bool of bool | String of string;;
type valueParameter =  Value of value;;
type parameter = ValueParam of valueParameter;;
type node = { name: string; param: parameter option; children: node list; };;

let test_p = ValueParam (Value (Float 3.4) ) ;; 
let test_n = { name = "foo"; param = Some test_p; children = [] } ;;
 
let pull p = match p with 
  | ValueParam v -> v ;;
let push p nv = match p with 
  | ValueParam (Value v) -> ValueParam (Value nv) ;;

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
type process = { };;
type interval = { minDuration:int; maxDuration : int option; nominalDuration : int; itvStatus: status; processes: process list };;

let condition = expression * interval list * interval list * status;


let state proc t = proc 
