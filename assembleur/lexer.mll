{
  open Lexing
  open Parser

  exception Lexing_error of string
  let lexing_error s = raise (Lexing_error s)

  let word_or_reg = function
    | "r0" -> Reg(0)
    | "r1" -> Reg(1)
    | "r2" -> Reg(2)
    | "r3" -> Reg(3)
    | "r4" -> Reg(4)
    | "r5" -> Reg(5)
    | "r6" -> Reg(6)
    | "r7" -> Reg(7)
    | "r8" -> Reg(8)
    | "r9" -> Reg(9)
    | "r10" -> Reg(10)
    | "r11" -> Reg(11)
    | "r12" -> Reg(12)
    | "r13" -> Reg(13)
    | "r14" -> Reg(14)
    | "r15" -> Reg(15)
    | "r16" -> Reg(16)
    | "r17" -> Reg(17)
    | "r18" -> Reg(18)
    | "r19" -> Reg(19)
    | "r20" -> Reg(20)
    | "r21" -> Reg(21)
    | "r22" -> Reg(22)
    | "r23" -> Reg(23)
    | "r24" -> Reg(24)
    | "r25" -> Reg(25)
    | "r26" -> Reg(26)
    | "r27" -> Reg(27)
    | "r28" -> Reg(28)
    | "r29" -> Reg(29)
    | "r30" -> Reg(30)
    | "r31" -> Reg(31)
    | "zero" -> Reg(0)
    | "ip" -> Reg(1)
    | "flags" -> Reg(2)
    | "if" -> If
    | w -> Word(w)

let bin = (2, (fun c -> match c with
  | '0' -> Some 0
  | '1' -> Some 1
  | _ -> None))

let hex = (16, (fun c -> match c with
  | '0'..'9' -> Some (int_of_char c - int_of_char '0')
  | 'A'..'F' -> Some (int_of_char c - int_of_char 'A' + 10)
  | 'a'..'f' -> Some (int_of_char c - int_of_char 'a' + 10)
  | _ -> None))

let dec = (10, (fun c -> match c with
  | '0'..'9' -> Some(int_of_char c - int_of_char '0')
  | _ -> None))

let read_num (base,read_digit) s first =
  let n = String.length s in
  let base = Int32.of_int base in
  let rec read acc i = 
    if i = n then acc
    else match s.[i] with  
      | ' ' | '\t' -> read acc (i+1)
      | c -> (match read_digit c with
	  | Some(x) -> read (Int32.add (Int32.mul base acc) (Int32.of_int x)) (i+1)
	  | None -> raise (Invalid_argument ("Not a digit : "^String.make 1 c)))
  in
  read Int32.zero first


}

let hexdigit = ['0'-'9' 'a'-'f' 'A'-'F']
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let id = (alpha | ['_' '.']) (alpha | digit | ['_' '-' '.'])*

let number = (digit+)

let space = [' ' '\t']

rule main = parse
  | '\n' { Lexing.new_line lexbuf; Newline }
  | (id as i) ':' { Label(i) }
  | ";" { comment lexbuf }
  | "," { Comma }
  | "-" { Neg }
  | '0' 'b' (['0' '1' ' ']* ['0' '1']) as b { Int(read_num bin b 2) }
  | "0x" ((hexdigit | ' ')* hexdigit) as h { Int(read_num hex h 2) }
  | digit+ (' '* digit+)* as d { Int(read_num dec d 0) }
  | id as i { word_or_reg i }
  | space { main lexbuf }
  | eof { EOF }
and comment = parse
  | '\n' { Lexing.new_line lexbuf; Newline }
  | _ { comment lexbuf }
  | eof { main lexbuf }

{

  let final_newline lex  =
    let insert_eof = ref false in
    fun lexbuf ->
      if !insert_eof
      then (insert_eof := false; EOF)
      else match lex lexbuf with
        | EOF -> (insert_eof := true; Newline)
        | t -> t

  let negative lex = 
    fun lexbuf ->
      match lex lexbuf with
        | Neg ->
          begin  match lex lexbuf with
            | Int(i) -> Int(Int32.neg i)
            | _ -> failwith "How can you negate that which has no sign ?"
          end
        | t -> t

  let asm_lex = final_newline (negative main)

}
