module N = Numbered.Make(struct let unique = 42 end)

open N

let cm2 x =
  let (state, set_state) = register () in
  set_state (x ^^ state);
  let r = x &&& state in
  (state, r)

let cm3 x = 
  let (b0, set_b0) = register () in
  let (b1, set_b1) = register () in
  let o = b1 &&& x in
  set_b0 (nnn o &&& (b0 ^^ x));
  set_b1 (nnn o &&& (b1 ^^ (b0 &&& x)));
  (b0,b1,o)

let cm24 x =
  let (b0,o0) = cm2 x in
  let (b1,o1) = cm2 o0 in
  let (b2,o2) = cm2 o1 in
  let (b3,b4,o) = cm3 o2 in
  [b0;b1;b2;b3;b4;o]

let _ =
  serialize 1 (cm24 (input 0))
