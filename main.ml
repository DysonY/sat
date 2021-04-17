open Exp
open Token

let solve (s : string) : bool array =
  let tokens = Lex.tokenize s in
  let (ast, _) = Parse.parse tokens in
  Eval.find_config ast

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

(* exps for tests *)
(*
let not_gate = Not (Var 1)
let and_gate = And (Var 1, Var 2)
let nand_gate = Not (And (Var 1, Var 2))
let or_gate = Or (Var 1, Var 2)
let nor_gate = Not (Or (Var 1, Var 2))
let xor_gate = Or (And (Not (Var 1), Var 2), And (Var 1, Not (Var 2)))
let xnor_gate = Not (Or (And (Not (Var 1), Var 2), And (Var 1, Not (Var 2))))
*)

(* utility functions for tests *)
(*
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

let expect result =
  if not result then failwith "Error"

let expect_not result =
  if result then failwith "Error"

(* testing individual boolean functions *)
let test_not_gate () =
  print_string "NOT test:\n";
  print_config (find_config not_gate);
  print_char '\n'

let test_and_gate () =
  print_string "AND test:\n";
  print_config (find_config and_gate);
  print_char '\n'

let test_nand_gate () =
  print_string "NAND test:\n";
  print_config (find_config nand_gate);
  print_char '\n'

let test_or_gate () =
  print_string "OR test:\n";
  print_config (find_config or_gate);
  print_char '\n'

let test_nor_gate () =
  print_string "NOR test:\n";
  print_config (find_config nor_gate);
  print_char '\n'

let test_xor_gate () =
  print_string "XOR test:\n";
  print_config (find_config xor_gate);
  print_char '\n'

let test_xnor_gate () =
  print_string "XNOR test:\n";
  print_config (find_config xnor_gate);
  print_char '\n'

(* test *)
let _ =
  test_not_gate ();
  test_and_gate ();
  test_nand_gate ();
  test_or_gate ();
  test_nor_gate ();
  test_xor_gate ();
  test_xnor_gate ();
*)
