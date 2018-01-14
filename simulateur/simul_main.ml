open Types
module TypesOps = Types.Make(Ops)
module SimpleSimul = Simul.Make(Ops)


open TypesOps
open SimpleSimul
open Ops
open Reader

let char_to_bool = function
  | '0' -> false
  | '1' -> true 
  | _ -> failwith "Pas un bit"

let read_input b n i = 
  if n = 0 then Continue [||] else
  try
    if not b then Printf.printf "In(%d)> " i; flush stdout;
    Scanf.scanf " %s" (fun s -> Continue(Array.init (String.length s) (fun i -> char_to_bool s.[i])))
  with _ -> Stop

let input_array a i = a.(i)

let write_bits b i u = 
  if not b then Printf.printf "Out(%d)> " i;
  Array.iter (fun i -> print_string (if i then "1 " else "0 ")) u;
  print_newline ()

let basic_interact b = { reader = read_input b; 
		       writer = write_bits b
		     }

(* Utiliser de la ram, afficher les actions à chaque fois.
   Adresses hexadécimales *)


let find_depth net =
  let nodes = net.nodes in
  let n = Array.length nodes in
  let depths = Array.make n 0 in
  Array.iteri (fun i -> function
    | Input(_) -> ()
    | Get(_) -> ()
    | Calc(_,l) -> depths.(i) <- 
      1 + List.fold_left (fun m k -> max m depths.(k)) 0 l) nodes;
  Array.fold_left max 0 depths

(*
let _ = 
  let arg = Sys.argv.(1) in
  let net = read_file arg in
  let ram = (Array.length Sys.argv >= 4 && Sys.argv.(2) = "--ram") in
  let i = if ram then (Ram.ram_interact Sys.argv.(3))  else basic_interact in
  if Array.length Sys.argv >= 3 && Sys.argv.(2) = "--stats" 
  then Printf.printf "Depth: %d\nSize: %d\n" (find_depth net) (Array.length net.nodes)
  else (Printf.printf "#Input(%d)\n" net.inputs;
    simul_full net i)
*)

let print_stats net =
  Printf.printf "Depth: %d\nSize: %d\n" (find_depth net) (Array.length net.nodes)

open Arg

let _ =
  let usage_msg = "-n <num>, --nb_tours <num> Limite le nombre de pas de la simulation à <num>
-b, --bare Désactive le prompt
-s, --stats Affiche les statistiques du circuit
-r <fichier>, --ram <fichier> Utilise <fichier> en tant que RAM" in
  let nb_tours = ref (-1) in
  let ram = ref None in
  let bare = ref false in
  let fichier = ref "" in
  let stats = ref false in
  let use_time = ref false in
  let start_time = ref None in
  let args = ["-n", Set_int nb_tours, "";
              "--nb_tours", Set_int nb_tours, "";
              "-b", Set bare, "";
              "--bare", Set bare, "";
              "-s", Set stats, "";
              "--stats", Set stats, "";
              "-r", String (fun s -> ram := Some s), "";
              "--ram", String (fun s -> ram := Some s), "";
              "--time", Set use_time, "";
              "--start-time", Float (fun t -> start_time := Some t), ""
             ] in
  parse args ((:=) fichier) usage_msg;
  if !use_time then Printf.printf "Time enabled !\n";
  let i = match !ram with
    | None -> basic_interact !bare
    | Some r -> Ram.ram_interact !use_time !start_time r
  in
  let net = read_file !fichier in
  if !stats then print_stats net;
  if not !bare then Printf.printf "#Input(%d)\n" net.inputs;
  simul_full !nb_tours net i
