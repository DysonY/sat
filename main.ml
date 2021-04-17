open Eval
open Lex
open Parse

let solve (s : string) : bool array =
  let tokens = tokenize s in
  let (ast, _) = parse tokens in
  find_config ast

let print_result result =
  if result then print_string "1\n"
  else print_string "0\n"

let int_of_bool b =
  if b then '1'
  else '0'

let print_config (config : bool array) =
  for i = 0 to Array.length config - 1 do
    print_int (i + 1);
    print_string ": ";
    print_char (int_of_bool config.(i));
    print_char '\n'
  done

let main (s : string) =
  print_config (solve s)

let _ = main Sys.argv.(1)
