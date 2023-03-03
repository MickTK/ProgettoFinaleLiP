{
open Parser
}

let white = [' ' '\n' '\t']+
let letter = ['a'-'z' 'A'-'Z']
let chr = ['a'-'z' 'A'-'Z' '0'-'9']
let id = letter chr*
let num = ['0'-'9']|['1'-'9']['0'-'9']*

rule read = 
  parse
  | white { read lexbuf }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "[" { LARR }
  | "]" { RARR }
  | "true" { TRUE }
  | "false" { FALSE }
  | "not" { NOT }
  | "and" { AND }
  | "or" { OR }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { MUL }
  | "=" { EQ }
  | "<=" { LEQ }
  | "proc" { PROC }
  | "repeat" { REPEAT }
  | "forever" { FOREVER }
  | "break" { BREAK }
  | "skip" { SKIP }
  | ":=" { TAKES }
  | ";" { SEQ }
  | "array" { ARRAY }
  | "val" { VAL }
  | "ref" { REF }
  | "int" { INT }
  | id { ID (Lexing.lexeme lexbuf) }
  | num { CONST (Lexing.lexeme lexbuf) }
  | eof { EOF }
