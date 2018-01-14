module Make(Ops : sig type op val exec : op -> bool list -> bool end) = struct

  module TypesOps = Types.Make(Ops)
  open TypesOps
  open Types

(* Contrainte : si un noeud apparait dans les arguments d'un calcul, 
   il doit avoir un id inférieur *)

(*
simul_step : 'a circuit -> bool array -> bool array -> (bool array, bool array)
*)
  let collect vals addrs = 
    Array.init (Array.length addrs) (fun i -> vals.(addrs.(i)))


  let simul_step circuit inputs regvals = 
    let process nodevals noeud_courant = function 
      | Input(i) -> inputs.(i)
      | Get(i) -> regvals.(i)
      | Calc(op,args) ->
        (assert (List.for_all (fun j -> j < noeud_courant) args);
         Ops.exec op (List.map (fun j -> nodevals.(j)) args)) 
    in
    let nodevals = Array.make (Array.length circuit.nodes) false in
    Array.iteri (fun i _ -> nodevals.(i) <- process nodevals i (circuit.nodes.(i))) nodevals;
    let newregs = collect nodevals circuit.registers in
    let outputs = collect nodevals circuit.outputs in
    (newregs, outputs)
      
  let simul_full n circuit interact = 
    let rec simul regs i = if i = n then () else
      match interact.reader circuit.inputs i with
        | Stop -> ()
        | Continue input -> 
	  (let (newregs, out) = simul_step circuit input regs in
	   interact.writer i out;
	   simul newregs (i+1))
    in
    simul (Array.make (Array.length circuit.registers) false) 0

end
