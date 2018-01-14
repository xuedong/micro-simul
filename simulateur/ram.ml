
(* Format temporaire : 
   listes de words séparé par des retours à la ligne
   b000000 00000 00000 00000 00000 00000 0 <= LSB en premier
   xABCDEF01 <= ici aussi

  le reste est rempli de 0
*)
open Types
module A = Bigarray.Array1

let bin = (2, (fun c -> match c with
  | '0' -> Some 0
  | '1' -> Some 1
  | _ -> None))

let hex = (16, (fun c -> match c with
  | '0'..'9' -> Some (int_of_char c - int_of_char '0')
  | 'A'..'F' -> Some (int_of_char c - int_of_char 'A' + 10)
  | 'a'..'f' -> Some (int_of_char c - int_of_char 'a' + 10)
  | _ -> None))

let read_num (base,read_digit) s =
  let n = String.length s in
  let base = Int32.of_int base in
  let rec read acc mul i = 
    if i = n then acc
    else match s.[i] with  
      | ' ' | '\t' -> read acc mul (i+1)
      | c -> (match read_digit c with
	  | Some(x) -> read (Int32.add (Int32.mul mul (Int32.of_int x)) acc) 
	    (Int32.mul mul base) (i+1)
	  | None -> raise (Invalid_argument ("Not a digit : "^String.make 1 c)))
  in
  read Int32.zero Int32.one 1 

let read_word s =
  if String.length s = 0 then None
  else match s.[0] with
    | 'b' -> Some (read_num bin s)
    | 'x' -> Some (read_num hex s)
    | '#' -> None
    | _ -> raise (Invalid_argument "Line does not start with b, x or #")

(* Le premier argument donne la taille du tableau à allouer *)
let read_ram file =
  let f = open_in file in
  let n = int_of_string (input_line f) in
  let a = A.create Bigarray.int32 Bigarray.c_layout n in
  A.fill a Int32.zero;
  let rec read i ln =
    try match read_word (input_line f) with
      | Some(x) -> (A.set a i x; read (i+1) (ln+1))
      | None -> read i (ln+1)
    with End_of_file -> ()
      | Invalid_argument a -> 
	raise (Invalid_argument (a ^ " (was at line "^string_of_int ln^")"))
  in
  read 0 1;
  a
  (* Normalement ça devrait être initialisé à 0 ? *)

let encode_int32 x =
  let a = Array.make 32 false in
  let rec dec i y =
    if y = 0l then ()
    else (a.(i) <- Int32.rem y 2l <> 0l;
	  dec (i+1) (Int32.shift_right_logical y 1))
  in
  dec 0 x;
  a

let decode_int a = Array.fold_right 
  (fun k m -> Int32.add (Int32.mul 2l m) (if k then 1l else 0l))
  a 0l

let bits x = let a = encode_int32 x in
	     let s = String.make 32 '0' in
	     Array.iteri (fun i v -> s.[i] <- if v then '1' else '0') a;
	     s


let ram_interact use_time start_time file =
  let cur_time = Unix.time () in
  let time_offset = match start_time with
    | None -> Int32.zero
    | Some(t) -> Int32.of_float (t -. cur_time) in
  let a = read_ram file in
  let fetch1 = ref 0 in
  let fetch2 = ref 0 in
  { reader = (fun _ i ->
    (* Update the timer *)
    let time = Int32.add time_offset (Int32.of_float (Unix.time ())) in
    Printf.printf "The time is %ld\n" time;
    if use_time then A.set a 2 time;
    let (fv1, fv2) = A.get a !fetch1, A.get a !fetch2 in
    Printf.printf "Cycle %d\n" i;
    let pi k addr v = Printf.printf 
      "In%d (from %x): %lx %lu %ld %s\n" k addr v v v (bits v) in
    pi 1 !fetch1 fv1; pi 2 !fetch2 fv2;
    Continue(Array.append (encode_int32 fv1) (encode_int32 fv2)));
    writer = (fun i k -> 
      let (f1, f2, set, set_addr, set_value) =
	(Array.sub k 0 32, Array.sub k 32 32,
	 k.(64),
	 Array.sub k 65 32, Array.sub k 97 32) in
      (* NOTE : ça peut overflow *)
      fetch1 := Int32.to_int (decode_int f1); 
      fetch2 := Int32.to_int (decode_int f2);
      let set_addr = Int32.to_int (decode_int set_addr) in
      let v = decode_int set_value in
      if not set then Printf.printf "No set (%lx %lu %ld %s -> %x)\n" v v v (bits v) set_addr
      else Printf.printf "Out: %lx %lu %ld %s -> %x\n" v v v (bits v) set_addr;
      Printf.printf "New IP: %x\n" !fetch1;
      Printf.printf "Normal fetch: %x\n" !fetch2;
      Printf.printf "State: \n";
      for i = 0 to 31 do 
        let v = Array.sub k (129+i*32) 32 in
        let v = decode_int v in
        Printf.printf "r%2d = %lx %lu %ld %s\n" i v v v (bits v)
      done;
      if set then A.set a set_addr v;
      ignore (read_line ())
    )
  }
      
