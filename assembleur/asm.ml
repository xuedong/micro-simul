
type cond = Always | Null | Neq | Eq | Lt | Gt | Lte | Gte
type param = PLabel of string | PInt of Int32.t | PReg of int
type instr = cond * string * param list
type cmd = Label of string | Instr of instr

let cond_to_string = function
  | Always -> "always"
  | Null -> "null"
  | Neq -> "neq"
  | Eq -> "eq"
  | Lt -> "lt"
  | Gt -> "gt"
  | Lte -> "lte"
  | Gte -> "gte"

let param_to_string = function
  | PLabel s -> s
  | PReg i -> Printf.sprintf "r%d" i
  | PInt i -> Int32.to_string i

let asm_dump = List.map (function
  | Label(s) -> Printf.printf "%s:\n" s
  | Instr(c,i,p) -> let rec f = function 
      | [] -> () 
      | [x] -> Printf.printf " %s" (param_to_string x)
      | x::xs -> Printf.printf " %s," (param_to_string x); f xs
                in
                    Printf.printf "\tif %s %s" (cond_to_string c) i;
                    f p;
                    Printf.printf "\n"
)
