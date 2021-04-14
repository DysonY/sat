type token =
  | NUM of (int)
  | LPAREN
  | RPAREN
  | NOT
  | AND
  | OR

val exp :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> exp
