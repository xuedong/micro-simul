module N = Numbered.Make(struct let unique = 42 end)

open N

let clk2 () = 
  let (o,set_o) = register () in
  let (c,set_c) = register () in
  set_o c;
  set_c (nnn o);
  o

let _ =
  serialize 0 [clk2 ()]
