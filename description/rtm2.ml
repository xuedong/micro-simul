module N = Numbered.Make(struct let unique = 42 end)
open N

let rtm2 r0 r1 =
  let (tick, set_tick) = register () in
  set_tick ( nnn tick );
  let (reg1, set_reg1) = register () in
  set_reg1 (r1);
  let (reg2, set_reg2) = register () in
  set_reg2 (mux tick r0 reg1);
  (reg1, mux tick reg1 r0)


let _ = 
  let (r0, r1) = rtm2 (input 0) in
  serialize 1 [ r0; r1 ]
