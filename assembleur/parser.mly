%token <string> Word
%token <string> Label
%token <Int32.t> Int
%token <int> Reg
%token If
%token Comma
%token Newline
%token EOF
%token Neg // Ce token ne devrait jamais apparaitre dans les lexemes qu'on reçoit

%start file
%type <Asm.cmd list> file

%%

file:
  | Newline* c = label_or_instr* EOF
    {c}

label_or_instr:
  | l = Label Newline* { Asm.Label(l) }
  | i = instr Newline+ { Asm.Instr(i) }

instr:
  | c = cond i = Word l = separated_list(Comma, param) { (c,i,l) }

cond:
  | c = rcond? { match c with None -> Asm.Always | Some(c) -> c } 

rcond:
  | If w = Word {
    match w with 
      | "null" -> Asm.Null
      | "neq" -> Asm.Neq
      | "eq" -> Asm.Eq
      | "lte" -> Asm.Lte
      | "lt" -> Asm.Lt
      | "gte" -> Asm.Gte
      | "gt" -> Asm.Gt
      | _ -> failwith ("Invalid condition : " ^ w)
  }

param:
  | w = Word { Asm.PLabel(w) }
  | i = Int { Asm.PInt(i) }
  | r = Reg { Asm.PReg(r) }
