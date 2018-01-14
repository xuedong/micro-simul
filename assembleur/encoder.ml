open Asm       

let places = 
  [("cond", (0,3)); ("opcode", (3,3)); ("imm", (16,16));
   ("reg0", (6,5)); ("reg1", (11,5)); ("reg2", (16,5));
   ("reg3", (21,5)); ("reg4", (26,5));
   ("snc1", (26,1)); ("snc2", (27,1)); ("snc3", (28, 1));
   ("table", (21, 4)); ("signed", (26, 1));
   ("opt", (29, 3)); ("shopt", (21, 5));
   ("bitwise_type", (31,1))]
   
let instr_size = 32

let encode_binary signed size (n : Int32.t) = 
  let open Int32 in 
  let two = Int32.of_int 2 in
  let bits = Array.make size false in
  let rec encode i (n : Int32.t) = 
    if i = size then 
      (if n = zero || signed && n = minus_one then ()
       else failwith "encode_binary failed : integer too big")
    else (bits.(i) <- not (rem n two = Int32.zero);
          encode (i+1) (div (sub n (if rem n two = zero then zero else one)) two))
  in
  encode 0 n; bits

let encode_cond x = Int32.of_int (match x with
  | Null -> 0
  | Lt -> 1
  | Eq -> 2
  | Lte -> 3
  | Gt -> 4
  | Neq -> 5
  | Gte -> 6
  | Always -> 7
)

(* encode : (string * bool * int) list -> bool array *)
let encode l = 
  let instr = Array.make 32 false in
  let filled = Array.make 32 false in
  let add_value (field, signed, v) =
    let (start, size) = List.assoc field places in
    let bits = encode_binary signed size v in
    Array.iteri (fun i b -> assert (not filled.(start+i));
      filled.(start+i) <- true; instr.(start+i) <- b) bits
  in
  List.iter add_value l;
  instr

let rec assemble labels pos (c,i,p) =
  (* Plus qu'à écrire les 500 helpers ici *)
  let opcode n = [("opcode", false, Int32.of_int n)] in
  let opt n = [("opt", false, Int32.of_int n)] in
  let shopt n = [("shopt", false, Int32.of_int n)] in
  let immediate k = 
    try
      let PInt(i) = List.nth p k in
      [("imm", true, i)]
    with _ -> failwith "Invalid immediate"
  in
  let reg pos dest =
    try
      let PReg(r) = List.nth p pos in
      [("reg"^string_of_int dest, false, Int32.of_int r)]
    with _ -> failwith "Invalid register"
  in
  let sreg' signed pos dest =
    let dest = string_of_int dest in 
    try
      match List.nth p pos with
        | PReg(r) -> [("reg"^dest, false, Int32.of_int r); 
                      ("snc"^dest, false, Int32.zero)]
        | PInt(i) -> [("reg"^dest, signed, i); 
                      ("snc"^dest, false, Int32.one)]
    with _ -> failwith "Invalid register or constant"
  in
  let sreg, sureg = sreg' true, sreg' false in
  let mul_sgn i = [("snc1", false, if i then Int32.one else Int32.zero)] in
  let encode r = [encode (("cond", false, encode_cond c)::r)] in
  let encode_imm op = encode (opcode op @ reg 0 0 @ reg 1 1 @ immediate 2) in
  let encode_arith_tern op = encode (opcode 4 @ reg 0 0 @ sreg 1 1 
                                     @ sreg 2 2 @ sreg 3 3 @ opt op) in
  (* TODO : bien gérer la diférente signé/non signé *)
  let encode_shift signed op = encode (opcode 4 @ opt 5 @ reg 0 0 @ sreg' signed 1 1 
                                       @ sureg 2 2 @ shopt op) in
  match i with
    | "addlo" -> encode_imm 0
    | "addhi" -> encode_imm 1
    | "load" -> encode_imm 2
    | "store" -> encode_imm 3
    | "muladd" -> encode_arith_tern 0
    | "saradd" -> encode_arith_tern 1
    | "slladd" -> encode_arith_tern 2
    | "slradd" -> encode_arith_tern 3
    | "mul" | "muls" -> encode (opcode 4 @ opt 4 @ reg 0 0 @ reg 1 1 @ sreg 2 2
                         @ sreg 3 3 @ mul_sgn true)
    | "mulu" -> encode (opcode 4 @ opt 4 @ reg 0 0 @ reg 1 1 @ sreg 2 2 
                        @ sreg 3 3 @ mul_sgn false)
    (* ici : fin des opérations arithmétiques, décalages *)
    | "cmp" -> encode (opcode 5 @ opt 1 @ sreg 0 1 @ sreg 1 2)
    | "cmpu" -> encode (opcode 5 @ opt 0 @ sureg 0 1 @ sureg 1 2)
(*    | "bitwisei" -> encode (opcode 6 @ bitwise_type 0 
                              @ reg 0 0 @ sureg 1 1 @ sureg 2 2
                              @ bitwise_table ()) *)
    | ".word" | ".signed" as t -> begin
      match p with
        | [PInt(i)] -> [encode_binary (t = ".signed") 32 i]
        | [PLabel(l)] -> [encode_binary false 32 (Hashtbl.find labels l)]
    end
    | ".fill" -> begin
      match p with
        | [PInt(i)] -> let u = Array.make 32 false in
                       let i = Int32.to_int i in
                       Array.to_list (Array.make i u)
    end
    | ".align" -> begin
      match p with
          [PInt(i)] -> let u = Array.make 32 false in
                       let i = Int32.to_int i in
                       Array.to_list (Array.make ((-pos) mod i + i) u)
    end
    | "j" -> begin
      match p with
          [PLabel(l)] -> assemble labels pos (c, "addlo", [PReg(1); PReg(0); PInt(Hashtbl.find labels l)])
    end
    | "la" -> begin
      match p with
          [PReg(r);PLabel(l)] -> assemble labels pos (c, "addlo", [PReg(r); PReg(0); PInt(Hashtbl.find labels l)])
    end
    | "sar" -> encode_shift true 19 (* 1+2+16 *)
    | "sal" -> encode_shift true 18
    | "slr" -> encode_shift false 17
    | "sll" -> encode_shift false 16
    | "rotr" -> encode_shift false 21
    | "rotl" -> encode_shift false 20
(* Si on veut aligner, il faut garder la position *)
    (* TODO : finir de mettre les opérations nécessaires *)

(* Hypothèse simplificatrice : on encode tous les jump en 2 instructions *)
let compute_size pos (_,i,p) =
  match i with
    | "li" -> 2
    | "la" -> 1
    | "j" -> begin
      match p with
        | [PLabel(l)] -> 1
        | _ -> 1
    end
    (* Traitements spéciaux *)
    | ".word" | ".signed" -> 1
    | ".fill" -> begin
      match p with
        | PInt(i)::_ -> Int32.to_int i
        | _ -> failwith "Invalid argument in .fill"
    end
    | ".align" ->  begin
      match p with
        | [PInt(i)] when i <> Int32.zero -> 
          let i = Int32.to_int i in
          (-pos) mod i + i 
        | _ -> failwith "Invalid argument in .align"
    end
    | _ -> 1

(* On calcule où se retrouveront les labels. *)
let compute_labels is = 
  let labels = Hashtbl.create 17 in
  List.fold_left (fun pos -> function
    | Label(n) -> 
      if Hashtbl.mem labels n 
      then failwith ("Multiple definitions of label "^n)
      else (Hashtbl.add labels n (Int32.of_int pos); pos)
    | Instr(i) -> pos + compute_size pos i
  ) 0 is;
  labels

(* ast -> bool array list *)
let encode_asm_file is =
  let labels = compute_labels is in
  List.fold_left (fun a b -> b @ a) [] 
    (snd (List.fold_left 
            (fun (pos,cs) -> function
              | Instr(i) -> let r = assemble labels pos i in (pos+List.length r, r::cs)
              | Label _ -> (pos,cs))
            (0,[]) is))
