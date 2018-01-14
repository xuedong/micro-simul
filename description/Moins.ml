module N = Numbered.Make(struct let unique = 42 end)

open N

let moins x =
  let (c, set_c) = register () in
  let y = x ^^ c in
  set_c (x ||| y);
  y

let _ =
  serialize 1 [moins (input 0)]
