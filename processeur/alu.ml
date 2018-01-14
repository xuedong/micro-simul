module N = Numbered.Make(struct let unique = 42 end)
module Utils = Utils.Make(N)

open N
open Utils


let is_zero a = nnn (treeduce (|||) a)

(* Construction d'un début d'ALU *)
(* incr est utile pour trouver la nouvelle valeur d'IP.
   Pas besoin d'un additionneur parallèle dans ce cas *)
let incr a =
  let n = Array.length a in
  let b = Array.make n zero in
  let rec incr c i =
    if i = n then ()
    else (b.(i) <- c ^^ a.(i); incr (c &&& a.(i)) (i+1))
  in
  incr one 0;
  b

let neg x = incr (Array.map nnn x)

(*** Comparateur ***)
(* Sorties : premier bit = égalité,
   deuxième bit = si le deuxième est plus grand ou égal au premier *)
let rec wide_compare signed a b =
  assert (Array.length a = Array.length b);
  let n = Array.length a in
  if n = 1 then match signed with
    | Some(s) -> (nnn (a.(0) ^^ b.(0)), s ^^ b.(0))
    | None ->  (nnn (a.(0) ^^ b.(0)), b.(0))
  else let (al,ah) = halve a in
       let (bl,bh) = halve b in
       let (eql, ltl) = wide_compare None al bl in
       let (eqh, lth) = wide_compare signed ah bh in
       (eql &&& eqh, mux eqh lth ltl)

let compare signed a b =
  let (eq, lt) = wide_compare (Some signed) a b in
  [| eq ||| lt; eq ||| nnn lt |]

(**** 
      Additionneur parallèle, profondeur O(log n) 
****)

(*  n = 2 ^ k *)
let rec sub_parallel_adder a b = 
  (* on regarde la longueur, et on en déduit le truc *)
  assert (Array.length a = Array.length b);
  let n = Array.length a in
  if n = 1 then 
    let u = a.(0) ^^ b.(0) in
    (([|u|],a.(0) &&& b.(0)), ([|nnn u|], a.(0) ||| b.(0)))
  else
    let (a1,a2) = halve a in
    let (b1,b2) = halve b in
    let ((s10, c10), (s11, c11)) = sub_parallel_adder a1 b1 in
    let ((s20, c20), (s21, c21)) = sub_parallel_adder a2 b2 in
    ((Array.append s10 (wide_mux c10 s20 s21), mux c10 c20 c21),
     (Array.append s11 (wide_mux c11 s20 s21), mux c11 c20 c21))

(* overflow detection : 
   en non signé, il y a overflow si le dernier bit est set
   en signé, c'est plus compliqué
*)
let parallel_adder signed a b =
  let ((s0, c0), (s1, c1)) = sub_parallel_adder a b in
  let n = Array.length a in
  let signed_overflow = c0 ^^ a.(n-1) ^^ b.(n-1) ^^ s0.(n-1) in 
  (s0,  mux signed c0 signed_overflow)

(***
    Addition et multiplication fusionnée
    On laisse la détection d'overflow à l'ALU qui va se charger
    de tronquer ça
    renvoie a*b+c
***)
let full_adder a b c =  (a ^^ b ^^ c,
			(a &&& b) ||| ((a ||| b) &&& c))

let half_adder a b = (a ^^ b, a &&& b)

(* signed détermine si la multiplication est signée ou non
   (c'est utile que si on regarde les 32 bits supérieurs, dans
    ce cas ça demande de soustraire quelques trucs) *)
let fast_multiply_add signed a b c =
  let n = Array.length a in
  let m = Array.length b in
  let products = Array.make (n+m) [] in
  let neg_a = neg a in
  let neg_b = neg b in
  let ext_a = a.(31) &&& signed in
  let ext_b = b.(31) &&& signed in
  for i = 0 to n - 1 do
    products.(32+i) <- (ext_a &&& neg_b.(i))::(ext_b &&& neg_a.(i))::products.(32+i)
  done;
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      products.(i+j) <- (a.(i) &&& b.(j))::products.(i+j)
    done
  done;
  for i = 0 to n - 1 do
    products.(i) <- c.(i)::products.(i)
  done;
  let rec reduce nextlevel newlevel = function
    | [] when newlevel = [] -> (zero, nextlevel) (* Au cas où *)
    | [] -> reduce nextlevel [] newlevel
    | [a] when newlevel = [] -> (a, nextlevel)
    | [a] -> reduce nextlevel [] (a::newlevel)
    | [a; b] -> let (res, carry) = half_adder a b in
                reduce (carry::nextlevel) (res::newlevel) []
    | a::b::c::xs -> let (res, carry) = full_adder a b c in
                     reduce (carry::nextlevel) (res::newlevel) xs
  in
  let results = Array.make (n+m) zero in
  let rec fill_results i and_add = 
    if i = n+m then ()
    else (let (r, next_add) = reduce [] [] (and_add @ products.(i)) in
         results.(i) <- r; fill_results (i+1) next_add)
  in
  fill_results 0 [];
  results

(*** 
     Toutes les opérations avec des décalages 
***)


let gen_mask a =
  let n = Array.length a in
  let rec submask i = 
    if i = 0 then [| zero |]
    else let mask = submask (i-1) in
         let k = Array.length mask in
         wide_mux a.(i-1) (Array.append mask (Array.make k zero))
           (Array.append (Array.make k one) mask)
  in
  submask n

let rotate k a =
  let m = Array.length k in
  let n = Array.length a in
  let rec subrot i pow2i =
    if i = m then a
    else let b = subrot (i+1) (2*pow2i) in
         Array.init n (fun j -> mux k.(i)  b.(j) b.((j + pow2i) mod n))
  in
  subrot 0 1

(* set/clear et get demandent de décaler 1, le reste demande 
   de décaler le truc de départ 

   shopt.(0) = direction (0 = left, 1 = right, inversé par rapport à nos bits)
   shopt.(1) = type (0 = logique, 1 = arithmétique)
*)
let shiftrotate_unit a k shopt =
  let dir = shopt.(0) in
  let inv_k = neg k in
  let mask = Array.map ((^^) dir) (gen_mask (wide_mux dir k inv_k)) in
  let mask = wide_mux (is_zero k &&& shopt.(4) &&& shopt.(0)) mask (Array.make 32 zero) in
  let source = wide_mux (shopt.(3) ||| shopt.(4)) 
    (Array.append [| one |] (Array.make 31 zero)) a in
  let rotated = rotate (wide_mux dir inv_k k) source in
  (* Si 0 : mux rotated a (repeat 32 shopt.(1))
     Si 1 : shopt.(1) ^^ mux rotated zero a
     Si 2 : mux (shopt.(0) ^^ mask) shopt.(1) a
     Si 3 : mux (shopt.(0) ^^ mask) a (not a)
     Si 4 : mux (shopt.(0) ^^ mask) a.(31) rotated
     Si 5 : rotated
  *)
  let map_mux a b c = 
    assert (Array.length a = Array.length b && Array.length b = Array.length c);
    Array.init (Array.length a) (fun i -> mux a.(i) b.(i) c.(i))
  in
  wide_mux shopt.(4) 
    (wide_mux shopt.(3)
       (wide_mux shopt.(2) 
          (map_mux rotated a (Array.make 32 shopt.(1)))
          (Array.map ((^^)shopt.(1)) (map_mux rotated (Array.make 32 zero) a)))
       (wide_mux shopt.(2)
          (map_mux mask (Array.make 32 shopt.(1)) a)
          (map_mux mask a (Array.map nnn a))))
    (wide_mux shopt.(2)
       (map_mux (Array.map (fun x -> nnn x) mask) (Array.make 32 (shopt.(1) &&& shopt.(0) &&& a.(31))) rotated) 
       rotated)

let bitwise2 a b t = 
  Array.init 32 
    (fun i -> mux b.(i) (mux a.(i) t.(0) t.(1)) (mux a.(i) t.(2) t.(3)))

let bitwise3 a b c d = 
  Array.init 32
    (fun i -> mux c.(i) 
      (mux b.(i) (mux a.(i) d.(0) d.(1))
         (mux a.(i) d.(2) d.(3)))
      (mux b.(i) (mux a.(i) d.(4) d.(5))
         (mux a.(i) d.(6) d.(7))))

let bitwise mode in1 in2 in3 in4 =
  wide_mux mode (bitwise2 in1 in2 in3) (bitwise3 in1 in2 in3 in4)

(* ALU complet
   TODO: gérer correctement les signés/non signés
   et l'overflow
*)
let alu opt shopt mul_sgn in1 in2 in3 = 
  (* Multiplicateur *)
  let mul_no_add = nnn opt.(2) in
  let mul_sgn = opt.(2) &&& mul_sgn in
  let mul_out = fast_multiply_add mul_sgn in2 in3 (Array.map ((&&&) mul_no_add) in1) in
  let (mul_out0, mul_out1) = Array.sub mul_out 0 32, Array.sub mul_out 32 32 in
  (* Additions et shift *)
  let only_shift = opt.(2) in
  let shopt2 = [| opt.(0); nnn opt.(1);  zero; zero; one |] in
  let shopt = wide_mux opt.(2) shopt2 shopt in
  let shifted = shiftrotate_unit (wide_mux only_shift in2 in1) (Array.sub (wide_mux only_shift in3 in2) 0 5) shopt in
  let (add_out0, add_overflow) = parallel_adder one shifted 
    (wide_mux only_shift in1 (Array.make 32 zero)) in
  (* Choix entre les deux *)
  let which = opt.(0) ||| opt.(1) in
  let out0 = wide_mux which mul_out0 add_out0 in
  let out1 = mul_out1 in
  let overflow = which &&& add_overflow in
  (out0, out1, overflow)


let select k = 
  (* Met le k-ième bit à 1 *)
  let n = Array.length k in
  let rec sel i = 
    if i = 0 then [| one |]
    else let r = sel (i-1) in
         let z = Array.map (fun _ -> zero) r in
         wide_mux k.(i-1) (Array.append r z) (Array.append z r)
  in
  sel n


let fetch_reg regs k = 
  let sk = select k in
  treeduce (array_map2 (|||)) 
    (array_map2 (fun sel r -> Array.map ((&&&) sel) r) sk regs)

let write_reg k nval regs =
  let sk = select k in
  array_map2 (fun s v -> wide_mux s v nval) sk regs

let write_reg_static k nval regs =
  Array.init (Array.length regs) (fun i -> if i = k then nval else regs.(i))

let extend n a =
  let k = Array.length a in
  Array.init n (fun i -> if i < k then a.(i) else zero)

let sign_extend n a =
  let k = Array.length a in
  Array.init n (fun i -> if i < k then a.(i) else a.(k-1)) 

let check_cond flags cond = 
  (cond.(0) &&& cond.(1) &&& cond.(2))
    ||| (nnn cond.(0) &&& nnn cond.(1) &&& nnn cond.(2) &&& flags.(0))
    ||| (cond.(0) &&& flags.(1) &&& nnn flags.(2))
    ||| (cond.(1) &&& flags.(1) &&& flags.(2))
    ||| (cond.(2) &&& nnn flags.(1) &&& flags.(2))
  

let decode_instr registers instr =
  let opcode = Array.sub instr 3 3 in
  let snc = Array.sub instr 26 3 in
  let is_arith = nnn opcode.(0) &&& nnn opcode.(1) &&& opcode.(2) in
  let aropt = wide_mux is_arith [| zero; one; zero |] (Array.sub instr 29 3) in (* 010 = slladd *)
  let shopt = Array.sub instr 21 5 in
  let is_signed = nnn opcode.(0) ||| instr.(29) in
  let r0 = Array.sub instr 6 5 in
  (* calcul de in1 *)
  let use_snc1 = snc.(0) &&& opcode.(2) &&& 
    (nnn opcode.(1) ||| nnn instr.(31)) in
  let r1 = Array.sub instr 11 5 in
  let in1 = wide_mux use_snc1 (fetch_reg registers r1) 
    (wide_mux is_signed (extend 32 r1) (sign_extend 32 r1)) in
  (* calcul de in2 *)
  let use_snc2 = snc.(1) &&&  (nnn opcode.(1) ||| nnn instr.(31)) in
  let use_imm = nnn opcode.(2) in
  let (r2,imm) = (Array.sub instr 16 5, Array.sub instr 16 16) in
  let in2 = wide_mux use_imm
    (wide_mux use_snc2 (fetch_reg registers r2) 
       (wide_mux is_signed (extend 32 r2) (sign_extend 32 r2)))
    (sign_extend 32 imm) in
  (* calcul de in3 *)
  let use_const = nnn opcode.(2) in
  let const_is_16 = nnn opcode.(1) &&& opcode.(0) in
  let const_16 = let a = Array.make 32 zero in a.(4) <- one; a in
  let use_snc3 = (snc.(2) &&& nnn opcode.(1)) ||| 
      (opcode.(1) &&& nnn instr.(31)) in
  let r3 = Array.sub instr 21 5 in
  let in3 = wide_mux use_const 
    (wide_mux use_snc3 (fetch_reg registers r3) (sign_extend 32 r3)) 
    (wide_mux const_is_16 (Array.make 32 zero) const_16) in
  (* in4 *)
  let r4 = Array.sub instr 26 5 in 
  let in4 = fetch_reg registers r4 in
  let bitwise_mode = instr.(31) in
  (* output params *)
  (in1, in2, in3, in4, aropt, shopt, snc.(0), instr.(29),
   opcode.(2) &&& opcode.(0) &&& nnn opcode.(1),
   nnn (mux opcode.(2) opcode.(1) opcode.(0)),
   opcode.(2) &&& opcode.(1) &&& nnn opcode.(0),
   wide_mux (opcode.(2) ||| nnn opcode.(1)) (Array.make 5 zero) r0,
   wide_mux (opcode.(2) &&& nnn opcode.(1) &&& nnn opcode.(0) &&&
          aropt.(2) &&& nnn aropt.(1) &&& nnn aropt.(0)) 
     (Array.make 5 zero) r1,
   nnn opcode.(2) &&& opcode.(1) &&& opcode.(0),
   fetch_reg registers r0,
   bitwise_mode,
   wide_mux (nnn opcode.(2) &&& opcode.(1) &&& nnn opcode.(0)) (Array.make 5 zero) r0,
  nnn opcode.(2) &&& opcode.(1) &&& nnn opcode.(0))

(*** Calcule un cycle du CPU ***)
(* TODO : zero sur load ? *)
let cpu_cycle registers instr fetch_register fetch_result =
  let registers = write_reg fetch_register fetch_result registers in
  registers.(0) <- Array.make 32 zero;
  let do_exec = check_cond registers.(2) (Array.sub instr 0 3) in
  let instr = wide_mux do_exec (Array.make 32 zero) instr in
  (* Décodage de l'instruction :
     on doit déterminer ce qui passe dans in1, in2, in3, in4
      => on sort in1, in1u, in2, in2u, in3, in4
     si l'instruction est spéciale, l'immediate passe dans in3,
     le premier reste dans in1
  *)
  let (in1, in2, in3, in4, aropt, shopt, mul_sgn, cmp_signed,
       is_cmp, write_zero, what_out0, where_out0, where_out1,
       is_write, write_val, bitwise_mode, fetch_reg, is_read) = 
    decode_instr registers instr in
  (* On doit transmettre à la sortie :
      is_cond, what_out0, where_out0, where_out1,
      is_write, write_val
  *)
  let (ar_out0, ar_out1, _) = alu aropt shopt mul_sgn in1 in2 in3 in
  let cmp_flags = compare cmp_signed in1 in2 in
  let bit_out0 = bitwise bitwise_mode in1 in2 in3 in4 in
  let incr_ip = incr registers.(1) in
  (* Faire tout ce qui est demandé *)
  let out0 = wide_mux what_out0 ar_out0 bit_out0 in
  (* Les flags *)
  let newflags = Array.copy registers.(2) in
  newflags.(0) <- mux write_zero newflags.(0) (is_zero out0);
  for i = 1 to 2 do newflags.(i) <- mux is_cmp newflags.(i) cmp_flags.(i-1) done;
  (* Modification des registres, sortie *)
  let registers = write_reg_static 1 incr_ip registers in
  let registers = write_reg_static 2 newflags registers in
  let registers = write_reg where_out0 out0 registers in
  let registers = write_reg where_out1 ar_out1 registers in
  (registers, wide_mux is_read (Array.make 32 zero)
   out0, fetch_reg, is_write, out0, write_val)

let cpu_interface input = 
  let (instruction, fetch_result) = Array.sub input 0 32, Array.sub input 32 32 in
  let r = Array.init 32 (fun _ -> wide_register 32) in
  let (registers, set_registers) = 
    (Array.map fst r, Array.iteri (fun i v -> snd r.(i) v)) in
  let (fetch_dest, set_fetch_dest) = wide_register 5 in
  let (registers, fetch_addr, fetch_reg, is_write, write_addr, write_val) =
    cpu_cycle registers instruction fetch_dest fetch_result in
  set_registers registers;
  set_fetch_dest fetch_reg;
  Array.concat ([registers.(1); fetch_addr; [| is_write |];
                write_addr; write_val] @ (Array.to_list registers))

let _ =
  let i = wide_input 0 63 in
  let o = cpu_interface i in
  serialize 64 (Array.to_list o)

