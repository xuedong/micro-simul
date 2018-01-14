open Types
open Ops
module TypesOps = Types.Make(Ops)
open TypesOps

let list_large_map f l =
  let rec map acc = function
    |[] -> List.rev acc
    |x::xs -> map (f x::acc) xs
  in
  map [] l

let is_digit = function
  | '0'..'9' -> true
  | _ -> false

let read_digit = function
  | '0'..'9' as x -> int_of_char x - int_of_char '0'
  | _ -> invalid_arg "Digit expected"

let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false

let ignore str pred i f =
  let n = String.length str in
  let rec ignore i = 
    if i = n then f i
    else if pred str.[i] then ignore (i+1)
    else f i
  in
  ignore i

let read_number str i cont =
  let n = String.length str in
  let rec parse start i acc = 
    if i = n  || not (is_digit str.[i]) then 
      (if start then invalid_arg "No number parsed"
       else cont acc i)
    else parse false (i+1) (acc*10 + read_digit str.[i])
  in
  parse true i 0

let read_word str i cont =
  let n = String.length str in
  let rec parse start i = 
    if i = n || is_space str.[i] then 
      (if start then invalid_arg "No word parsed"
       else cont i)
    else parse false (i+1)
  in
  parse true i
  
let spaces_then f str i cont =
  ignore str is_space i (fun j -> f str j (cont j))

(* CHECKME : pas de \n à la fin des chaines ? *)
let read_file filename = 
  let fd = open_in filename in
  let rec lines acc = 
    match (try Some(input_line fd) with End_of_file -> None) with
      |Some(l) -> lines (l::acc)
      |None -> List.rev acc
  in
  let lines = lines [] in
  let drop_comments str = 
    let n = String.length str in
    let rec search_end k i = 
      if i = n then k
      else if str.[i] = '#' then k
      else if str.[i] =' ' || str.[i] = '\t' then search_end k (i+1)
      else search_end (i+1) (i+1)
    in
    let end_pos = search_end 0 0 in
    String.sub str 0 end_pos
  in
  let uncommented = 
    List.filter (fun s -> String.length s > 0) 
    (list_large_map drop_comments lines) 
  in
  (* Ensuite, on lit les trucs dans l'ordre : la ligne Input, 
     la ligne Output et ses arguments, la ligne Registers et ses arguments,
     la ligne donnant le nombre de Nodes, et on parse la liste des noeuds*)
  (* Format : mot (a-z-, commence par une majuscule), 
     puis des trcs entre parenthèses (peut-être), suivis d'un autre truc *)
  let parse_line str = 
    let n = String.length str in
    let rec get_name i = 
      if str.[i] = '(' then parse_length i (i+1)
      else if i = n || is_space str.[i] || str.[i] = ':' || str.[i] = '(' 
      then ignore str is_space i
        (fun k -> if k = n then (i, None, n)
          else if str.[k] = '(' then parse_length i (k+1)
          else if str.[k] = ':' then parse_arg i None (k+1)
          else invalid_arg "Spaces in the name of a property ?")
      else get_name (i+1)
    and parse_length i i' = 
      (* Tellement gore. Tout renommer en "pos" et masquer les bindings ? *)
      ignore str is_space i' (fun j ->
	read_number str j (fun len k ->
	  ignore str is_space k (fun l -> 
	    if l = n || str.[l] <> ')' 
	    then invalid_arg "Missing closing parenthese"
	    else ignore str is_space (l+1) (fun m ->
	      if m = n then (i, Some(len), n)
	      else if str.[m] = ':' then parse_arg i (Some len) (m+1)
	      else invalid_arg "Expected EOL or : after length"
       ))))
    and parse_arg i len j = 
      ignore str is_space j (fun k -> (i, len, k))
    in
    let (i,len,k) = get_name 0 in
    (String.sub str 0 i, len, String.sub str k (n-k))
  in
  (* Parser un truc de la forme 0:a, 1:b, 2:c ... *)
  let parse_array length str =
    let n = String.length str in
    let rec do_parse acc next_idx i = 
      ignore str is_space i (fun i ->
	if i = n then (
	  if acc = [] then ([], next_idx) 
	  else invalid_arg "End of string, number expected")
	else read_number str i (fun key i ->
	  ignore str is_space i (fun i ->
	    if i = n || str.[i] <> ':' then invalid_arg "Expected :"
	    else ignore str is_space (i+1) (fun i ->
	      if i = n then invalid_arg "Unexpected end of string"
	      else read_number str i (fun value i ->
		if key = next_idx then parse_next ((key,value)::acc) (next_idx+1) i
		else invalid_arg "Indices not consecutive"
      )))))
    and parse_next acc next_idx i =
      ignore str is_space i (fun i ->
	if i = n then (acc, next_idx)
	else if str.[i] = ',' then do_parse acc next_idx (i+1)
	else invalid_arg "Expected ,"
      )
    in
    let (list, last) = do_parse [] 0 0 in
    if last <> length
    then invalid_arg "Not enough elements"
    else
      let a = Array.make length 0 in
      (List.iter (fun (i,v) -> a.(i) <- v) list;
       a)
  in
  let parse_instr idx str = 
    let n = String.length str in
    let rec parse_many_ints acc i = 
      ignore str is_space i (fun i ->
	if i = n then List.rev acc
	else read_number str i (fun node i -> parse_many_ints (node::acc) i))
    in
    spaces_then read_number str 0 (fun _ key i ->
      if key <> idx then invalid_arg "Bad index"
      else ignore str is_space i (fun i ->
	if i = n || str.[i] <> ':' then invalid_arg "Syntax error"
	else spaces_then read_word str (i+1) (fun j k ->
	  let w = String.sub str j (k-j) in
	  match w with
	    | "Input" -> (match parse_many_ints [] k with
		| [x] -> Input(x)
		| _ -> invalid_arg "Input requires one argument")
	    | "Get" -> (match parse_many_ints [] k with
		| [x] -> Get(x)
		| _ -> invalid_arg "Get requires one argument")
	    | "Calc" ->  let i = k in 
	      spaces_then read_word str i (fun j k ->
		let op_name = String.sub str j (k-j) in
		let (op, num_args) = op_from_name op_name in
		let args = parse_many_ints [] k in
		if List.length args <> num_args
		then invalid_arg "Wrong number of arguments"
		else Calc(op, args)
	      )
	    | n -> invalid_arg ("Not a valid node" ^ n))))
  in
  (* Combiner toutes ces fonctions *)
  let input, output, registers, nodes, nodevals = match uncommented with
    | i::o::r::n::nv -> i, o, r, n, nv
    | _ -> failwith "Invalid file"
  in
  let nb_input = match parse_line input with
    | ("Input", Some nb_input, _) -> nb_input
    | (s, _, _) -> failwith ("Expected Input but got " ^ s)
  in
  let nb_output, str_output = match parse_line output with
    | ("Output", Some nb_output, str_output) -> nb_output, str_output
    | (s, _, _) -> failwith ("Expected Output but got " ^ s)
  in
  let array_output = parse_array nb_output str_output in
  let nb_registers, str_registers = match parse_line registers with
    | ("Registers", Some nb_registers, str_registers) -> nb_registers, str_registers
    | (s, _, _) -> failwith ("Expected Registers but got " ^ s)
  in
  let array_registers = parse_array nb_registers str_registers in
  let nb_nodes = match parse_line nodes with
    | ("Nodes", Some nb_nodes, _) -> nb_nodes
    | (s, _, _) -> failwith ("Expected Nodes but got " ^ s)
  in
  let array_nodes = Array.mapi (fun i s -> parse_instr i s) (Array.of_list nodevals) in
  if Array.length array_nodes <> nb_nodes
  then invalid_arg "Wrong number of nodes !"
  else { registers = array_registers;
	 inputs = nb_input;
	 outputs = array_output;
	 nodes = array_nodes
       }


let output_array f unpack arr =
  let n = Array.length arr in
  let rec output k =
    if k = n then Printf.fprintf f "\n"
    else (Printf.fprintf f " %d:%d" k (unpack arr.(k));
	  if k <> n-1 then Printf.fprintf f ",";
	  output (k+1))
  in
  output 0

(* TODO : vérifier le nombre d'entrées,
   le fait que les registres ont des entrées *)
let write_file c f =
  let outputs_nb = Array.length c.outputs in
  Printf.fprintf f "Input(%d)\n" c.inputs;
  Printf.fprintf f "Output(%d):" outputs_nb;
  output_array f (fun k -> k) c.outputs;
  Printf.fprintf f "Registers(%d):" (Array.length c.registers);
  output_array f (fun k -> k) c.registers;
  let nodes = c.nodes in
  Printf.fprintf f "Nodes(%d):\n" (Array.length nodes); 
  Array.iteri (fun i -> function
    | Input(j) -> Printf.fprintf f "%d: Input %d\n" i j
    | Get(j) -> Printf.fprintf f "%d: Get %d\n" i j
    | Calc(s,r) -> (Printf.fprintf f "%d: Calc %s" i (name_from_op s);
		    List.iter (Printf.fprintf f " %d") r;
		    Printf.fprintf f "\n")) nodes


