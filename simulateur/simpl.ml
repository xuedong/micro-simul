open Types
module TypeOps = Types.Make(Ops)
open TypeOps
open Ops

let from_some = function
  | Some(x) -> x
  | None -> raise (Invalid_argument "from_some")

let truncate c = 
  let nodes = c.nodes in
  let n = Array.length c.nodes in
  let used = Array.make n false in
  let rec explore i =
    if used.(i) then ()
    else (used.(i) <- true;
          match nodes.(i) with
            | Calc(_,l) -> List.iter explore l
            | _ -> ()
    )
  in
  Array.iter explore c.outputs;
  Array.iter explore c.registers;
  let renum = Array.make n None in
  let newnodes = Array.make n (Input(0)) in
  let rename = function 
    | Calc(o,l) -> Calc(o,List.map (fun i -> from_some renum.(i)) l) 
    | x -> x
  in
  let rec replace i k = 
    if i = n then k
    else if used.(i) then (newnodes.(k) <- rename nodes.(i);
                           renum.(i) <- Some(k);
                           replace (i+1) (k+1))
    else replace (i+1) k
  in
  let num = replace 0 0 in
  Printf.printf "From %d to %d nodes (deleted %d)\n" n num (n-num);
  { 
    nodes = Array.sub newnodes 0 num;
    inputs = c.inputs;
    outputs = Array.map (fun i -> from_some renum.(i)) c.outputs;
    registers = Array.map (fun i -> from_some renum.(i)) c.registers
  }


type nt = Copy of int | Node of node

let reduce c =
  let nodes = c.nodes in
  let n = Array.length c.nodes in
  let vals = Array.init n (fun i -> Copy(i)) in
  let getvalid i = match vals.(i) with
    | Node n -> (i,n)
    | Copy j -> match vals.(j) with
        | Node n -> (j,n)
        | Copy _ -> failwith ("Copie de copie ? " ^ string_of_int i ^ " " ^string_of_int j)
  in
  let simpl i = 
    vals.(i) <- match nodes.(i) with
      | Input(k) -> Node(nodes.(i))
      | Get(k) -> Node(nodes.(i))
      | Calc(op, l) -> 
        let v = List.map (fun k -> getvalid k) l in
        match (op, v) with
          | (Not, [(_,Calc(Zero,_))]) -> Node(Calc(One,[]))
          | (Not, [(_,Calc(One,_))]) -> Node(Calc(Zero,[]))
          | (Or, [(_,Calc(Zero,_));(j,_)]) 
          | (Or, [(j,_);(_,Calc(Zero,_))]) -> Copy(j)
          | (Or, [(_,Calc(One,_));_])
          | (Or, [_;(_,Calc(One,_))]) -> Node(Calc(One,[]))
          | (And, [(_,Calc(Zero,_));_])
          | (And, [_;(_,Calc(Zero,_))]) -> Node(Calc(Zero,[]))
          | (And, [(_,Calc(One,_));(j,_)])
          | (And, [(j,_);(_,Calc(One,_))]) -> Copy(j)
          | (Xor, [(j,_); (_,Calc(Zero,_))])
          | (Xor, [(_,Calc(Zero,_));(j,_)]) -> Copy(j)
          | (Xor, [(_,Calc(One,_));(j,_)])
          | (Xor, [(j,_);(_,Calc(One,_))]) -> Node(Calc(Not,[j]))
          | (Mux, [(_,Calc(Zero,_)); (j,_); _]) -> Copy(j)
          | (Mux, [(_,Calc(One,_)); _; (j,_)]) -> Copy(j)
          | (o, l) -> Node(Calc(o,List.map fst l))
  in
  Array.iteri (fun i _ -> simpl i) nodes;
  let copies = Array.fold_left (fun c -> function | Copy _ -> 1+c | Node _ -> c) 0 vals in
  let consts = Array.fold_left (fun c -> function 
    | Copy _ -> c 
    | Node(Calc(One,_)) | Node(Calc(Zero,_)) -> 1+c
    | Node _ -> c
  ) 0 vals in
  Printf.printf "Started with %d nodes, got %d copies and %d constants (remaining : %d nodes)\n"
    n copies consts (n - copies - consts);
  {c with nodes = Array.map (function
    | Node n -> n
    | Copy i -> Calc(Id,[i])) vals }

(* On attribue des numéros aux noeuds.
   Faire ça rapidement (en O(n) plutôt que O(n²)) : on fait une hashtable contentant les (ops, [noeuds]) ou input i ou get i
*)
let cse c =
  let nodes = c.nodes in
  let n = Array.length nodes in
  let hash = Hashtbl.create n in
  let nums = Array.init n (fun i -> i) in
  let renum_node = function
    | Input i -> Input i
    | Get j -> Get j
    | Calc(op,l) -> Calc(op, List.map (fun k -> nums.(k)) l)
  in
  Array.iteri (fun i x ->
    let x = renum_node x in
    if Hashtbl.mem hash x then
      nums.(i) <- Hashtbl.find hash x
    else
      Hashtbl.add hash x i)
    nodes;
  (* Là, nums contient le numéro du premier noeud qui nous intéresse *)
  let renum = Array.init n (fun i -> i) in
  let newnodes = Array.make (Hashtbl.length hash) (Calc(Zero,[])) in
  let renum_node = function
    | Calc(op,l) -> Calc(op, List.map (fun k -> renum.(k)) l)
    | x -> x
  in
  let rec copy i k = 
    if i = n then (assert (k = Hashtbl.length hash))
    else if nums.(i) = i 
    then (renum.(i) <- k; newnodes.(k) <- renum_node nodes.(i); copy (i+1) (k+1))
    else (renum.(i) <- renum.(nums.(i)); copy (i+1) k)
  in
  copy 0 0;
  Printf.printf "CSE : from %d to %d nodes\n" n (Hashtbl.length hash);
  { nodes = newnodes;
    inputs = c.inputs;
    outputs = Array.map (fun i -> renum.(i)) c.outputs;
    registers = Array.map (fun i -> renum.(i)) c.registers
  }

let _ = 
  let infile = Sys.argv.(1) in
  let c = Reader.read_file infile in
  let c' = truncate (reduce c) in
  let c'' = truncate (cse c') in
  let outfile = open_out Sys.argv.(2) in
  Reader.write_file c'' outfile
