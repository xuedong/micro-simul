(* Tableau extensible, insertion en O(1) amorti *)
type 'a vector = (('a option) array * int) ref

let empty () = ref ([| None |], 0)
  
let size vect = 
  let (_,size) = !vect in size
				     
let insert vect x = 
  let (arr, size) = !vect in
  let arrsize = Array.length arr in
  let newarr = if arrsize > size then arr
    else Array.init (arrsize*2) (fun i -> if i < size then arr.(i) else None)
  in
  newarr.(size) <- Some(x);
  vect := (newarr, size+1);
  size
    
let idx vect i = 
  let (arr, size) = !vect in 
  if i >= size then raise (Invalid_argument "index out of bounds")
  else let (Some x) = arr.(i) in x
			      
let update vect i x =
  let (arr, size) = !vect in
  if i >= size then raise (Invalid_argument "index out of bounds")
  else arr.(i)<-Some(x)
    
let freeze vect = 
  let (arr, size) = !vect in
  Array.init size (fun i -> match arr.(i) with
    | Some(x) -> x
    | None -> failwith "Ne devrait pas arriver")

let (@!) vect i = idx vect i
