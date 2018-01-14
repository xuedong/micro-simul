
type op = Not | Or | And | Xor | Mux | Zero | One | Id

(* type op_descr = *)

let nullop v = function
  | [] -> v
  | _ -> failwith "0 operandes attendues"
let unop f = function
  | [x] -> f x
  | _ -> failwith "1 operande attendue"
let binop f = function
  | [x;y] -> f x y
  | _ -> failwith "2 operandes attendues"
let terop f = function
  | [x;y;z] -> f x y z
  | _ -> failwith "3 operandes attendues"

let ops = function
  | Zero -> nullop false
  | One -> nullop true
  | Not -> unop not
  | Or -> binop (fun x y -> x || y)
  | And -> binop (fun x y -> x && y)
  | Xor -> binop (fun x y -> x <> y)
  | Mux -> terop (fun x y z -> if x then z else y)
  | Id -> unop (fun x -> x)

let op_from_name = function
  | "Zero" -> (Zero, 0)
  | "One" -> (One, 0)
  | "Not" -> (Not, 1)
  | "Or" -> (Or, 2)
  | "And" -> (And, 2)
  | "Xor" -> (Xor, 2)
  | "Mux" -> (Mux, 3)
  | "Id" -> (Id, 1)
  | _ -> raise (Invalid_argument "Not an operator")

let name_from_op = function
  | Zero -> "Zero"
  | One -> "One"
  | Not -> "Not"
  | Or -> "Or"
  | And -> "And"
  | Xor -> "Xor"
  | Mux -> "Mux"
  | Id -> "Id"

let exec = ops

