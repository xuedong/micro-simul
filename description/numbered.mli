module type S = 
sig
(*  type op = Input of int | RegInp of int | Calc of (string * int list) *)
  type noderef (* = Noderef of int *)
(*  val get_node_id : noderef -> int
  val nodes : op Vector.vector
  val registers : int option Vector.vector
  val make_node : op -> noderef
  val make_op : string -> noderef list -> noderef *)
  val zero : noderef
  val one : noderef
  val ( ||| ) : noderef -> noderef -> noderef
  val ( &&& ) : noderef -> noderef -> noderef
  val ( ^^ ) : noderef -> noderef -> noderef
  val mux: noderef -> noderef -> noderef -> noderef 
  val nnn : noderef -> noderef
  val input : int -> noderef
  val register : unit -> noderef * (noderef -> unit)
  val serialize : int -> noderef list -> unit
end

module Make :
  functor (Unique : sig val unique : int end) -> S
