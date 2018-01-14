module N = Numbered.Make(struct let unique = 42 end)

open N

let full_adder a b c = 
  (a ^^ b ^^ c,
   (a &&& b) ||| ((a ||| b) &&& c))

let _ =
  let (s, r) = full_adder (input 0) (input 1) (input 2) in
  serialize 3 [ s; r ]

