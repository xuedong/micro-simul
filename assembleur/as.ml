open Printf
open Parser
open Format
open Lexing

let string_of_lexeme = function
  | Word(s) -> sprintf "Word(%S)" s
  | Label(s) -> sprintf "Label(%S)" s
  | Reg(i) -> sprintf "Reg(%d)" i
  | Int(i) -> sprintf "Int(%d)" (Int32.to_int i)
  | Comma -> "Comma"
  | Newline -> "Newline"
  | EOF -> "EOF"
  | If -> "If"
  | Neg -> "Neg"

let localisation pos =
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
  eprintf "Ligne %d, caractères %d-%d:\n" l (c-1) c

let _ =
  let lexbuf = Lexing.from_channel stdin in
  try
    let ast = Parser.file Lexer.asm_lex lexbuf in
    let encoded = Encoder.encode_asm_file ast in
    Printf.printf "%d\n" (List.length encoded);
    List.map (fun a -> print_string "b"; Array.iter (fun t -> print_int (if t then 1 else 0)) a;
      print_newline ()) encoded
  with
    | Parser.Error ->
      localisation (Lexing.lexeme_start_p lexbuf);
      eprintf "Erreur dans l'analyse syntaxique@.";
      exit 1
