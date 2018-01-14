module Make(N : Numbered.S) = struct

  open N

  (* Déficiences de la lib standard d'ocaml *)
  let list_init n f = Array.to_list (Array.init n f)

  let array_map2 f a b =
    assert (Array.length a = Array.length b);
    Array.init (Array.length a) (fun i -> f a.(i) b.(i))
    
  let rec exp2 = function
    | 0 -> 1
    | n when n < 0 -> raise (Invalid_argument "2^negative number")
    | n when n mod 2 = 0 -> let u = exp2 (n/2) in u*u
    | n when n mod 2 = 1 -> 2 * exp2 (n-1)
    | _ -> 42

  (* rotation de k vers la gauche (LSB à gauche) *)
  let rotate_array k a = 
    let n = Array.length a in
    Array.init n (fun i -> a.((i+k) mod n))


  (* Versions "wide" opérant sur plusieurs bits à la fois *)
  let wide_register width = 
    let values = Array.make width zero in
    let setters = Array.init width 
      (fun i -> let (node, setter) = register () in
		values.(i) <- node; setter) in
    (values, (fun a -> 
      assert (Array.length a = width);
      Array.iteri (fun i -> setters.(i)) a))

  let wide_input i j = Array.init (j-i+1) (fun k -> input (i+k))

  let wide_mux c a b = array_map2 (mux c) a b

  (* Parallélisme *)

  (* Découpage en deux parties égales *)
  let halve a = 
    let n = Array.length a in
    assert ( n mod 2 = 0 );
    let m = n/2 in
    (Array.sub a 0 m, Array.sub a m m)
      
  (* Découpage en deux parties presque égales *)
  let quasi_halve a =
    let n = Array.length a in
    let m = n/2  in
    (Array.sub a 0 (n-m), Array.sub a (n-m) m)

  (* Réduction parallèle *)
  let rec treeduce op a = 
    let n = Array.length a in
    if n = 0 then raise (Invalid_argument "Empty array")
    else if n = 1 then a.(0)
    else let (a1,a2) = quasi_halve a in
	 op (treeduce op a1) (treeduce op a2)


end
