module Make (Ops : sig type op val exec : op -> bool list -> bool end) = struct
    
  type node = 
    | Input of int 
    | Calc of Ops.op * int list 
    | Get of int 
        
        
  type circuit = { registers : int array;
		   inputs : int;
		   outputs : int array;
		   nodes : node array 
		 }

end
  
  
type 'a cont = Continue of 'a | Stop
      
type interact = { reader : int -> int -> (bool array) cont;
                  writer : int -> bool array -> unit
		}
