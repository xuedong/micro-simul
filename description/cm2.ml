module N = Numbered.Make(struct let unique = 42 end)
open N

let cm2 x =
  let (state, set_state) = register () in
  set_state (x ^^ state);
  let r = x &&& state in
  (state, r)

let _ = 
  let (s,r) = cm2 (input 0) in
  serialize 1 [ s; r ]
