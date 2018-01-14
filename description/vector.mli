type 'a vector
val empty : unit -> 'a vector
val size : 'a vector -> int
val insert : 'a vector -> 'a -> int
val idx : 'a vector -> int -> 'a
val update : 'a vector -> int -> 'a -> unit
val freeze : 'a vector -> 'a array
val ( @! ) : 'a vector -> int -> 'a
