module type S = 
sig
  type noderef
  val zero : noderef
  val one : noderef
  val ( ||| ) : noderef -> noderef -> noderef
  val ( &&& ) : noderef -> noderef -> noderef
  val ( ^^ ) : noderef -> noderef -> noderef
  val mux : noderef -> noderef -> noderef -> noderef
  val nnn : noderef -> noderef
  val input : int -> noderef
  val register : unit -> noderef * (noderef -> unit)
  val serialize : int -> noderef list -> unit
end

module Make(Unique : sig val unique : int end) = struct
open Vector

type op = Input of int | RegInp of int | Calc of (string * int list)
type noderef = Noderef of int

let get_node_id (Noderef(i)) = i

(* Un "vector" en ocaml ? *)
let (nodes : op Vector.vector) = Vector.empty ()
let (registers : (int option) Vector.vector) = Vector.empty ()

let make_node op = Noderef(Vector.insert nodes op)

let make_op s l = make_node (Calc(s, List.map get_node_id l))

(* zero et one prennent les deux premiers nodes *)
let zero = make_op "Zero" []
let one = make_op "One" []
let (|||) a b = make_op "Or" [a; b] 
let (&&&) a b = make_op "And" [a; b]
let (^^) a b = make_op "Xor" [a; b]
let mux a b c = make_op "Mux" [a; b; c]
let nnn a = make_op "Not" [a]
let input i = make_node (Input(i))

let register () = 
  let regid = Vector.insert registers None in
  (make_node (RegInp(regid)), (fun (Noderef i) -> 
    match Vector.idx registers regid with
      | None -> Vector.update registers regid (Some(i))
      | Some(_) -> invalid_arg "register already set"))

(* (double-)fonctoriser pour gérer les effets de bord *)

let output_array unpack arr =
  let n = Array.length arr in
  let rec output k =
    if k = n then Printf.printf "\n"
    else (Printf.printf " %d:%d" k (unpack arr.(k));
	  if k <> n-1 then Printf.printf ",";
	  output (k+1))
  in
  output 0

(* TODO : vérifier le nombre d'entrées *)
let serialize inputs_nb outputs =
  let outputs_nb = List.length outputs in
  Printf.printf "Input(%d)\n" inputs_nb;
  Printf.printf "Output(%d):" outputs_nb;
  output_array (get_node_id) (Array.of_list outputs);
  Printf.printf "Registers(%d):" (Vector.size registers);
  let check_register = function
    | Some k -> k
    | None -> failwith "register without input"
  in
  output_array check_register (Vector.freeze registers);
  let nodes = Vector.freeze nodes in
  Printf.printf "Nodes(%d):\n" (Array.length nodes); 
  Array.iteri (fun i -> function
    | Input(j) -> Printf.printf "%d: Input %d\n" i j
    | RegInp(j) -> Printf.printf "%d: Get %d\n" i j
    | Calc(s,r) -> (Printf.printf "%d: Calc %s" i s;
		    List.iter (Printf.printf " %d") r;
		    Printf.printf "\n")) nodes


end
