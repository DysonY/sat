type token =
  | NUM of (int)
  | LPAREN
  | RPAREN
  | NOT
  | AND
  | OR

open Parsing;;
let _ = parse_error;;
# 2 "grammar.mly"
  type exp =
  | Num of int
  | Block of exp
  | And of exp * exp
  | Or of exp * exp
  | Not of exp
# 19 "grammar.ml"
let yytransl_const = [|
  258 (* LPAREN *);
  259 (* RPAREN *);
  260 (* NOT *);
  261 (* AND *);
  262 (* OR *);
    0|]

let yytransl_block = [|
  257 (* NUM *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\001\000\001\000\000\000"

let yylen = "\002\000\
\001\000\003\000\003\000\003\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\001\000\000\000\000\000\000\000\000\000\005\000\
\000\000\000\000\002\000\003\000\004\000"

let yydgoto = "\002\000\
\006\000"

let yysindex = "\003\000\
\006\255\000\000\000\000\006\255\006\255\253\254\008\255\000\000\
\006\255\006\255\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\009\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\252\255"

let yytablesize = 14
let yytable = "\007\000\
\008\000\009\000\010\000\001\000\012\000\013\000\003\000\004\000\
\006\000\005\000\011\000\000\000\009\000\010\000"

let yycheck = "\004\000\
\005\000\005\001\006\001\001\000\009\000\010\000\001\001\002\001\
\000\000\004\001\003\001\255\255\005\001\006\001"

let yynames_const = "\
  LPAREN\000\
  RPAREN\000\
  NOT\000\
  AND\000\
  OR\000\
  "

let yynames_block = "\
  NUM\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 20 "grammar.mly"
        ( Num _1 )
# 84 "grammar.ml"
               : exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : exp) in
    Obj.repr(
# 22 "grammar.mly"
                    ( Block _2 )
# 91 "grammar.ml"
               : exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : exp) in
    Obj.repr(
# 24 "grammar.mly"
               ( And (_1, _3) )
# 99 "grammar.ml"
               : exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : exp) in
    Obj.repr(
# 26 "grammar.mly"
              ( Or (_1, _3) )
# 107 "grammar.ml"
               : exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : exp) in
    Obj.repr(
# 28 "grammar.mly"
           ( Not _2 )
# 114 "grammar.ml"
               : exp))
(* Entry exp *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let exp (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : exp)
;;
