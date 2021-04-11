open Exp

let gen_empty (num_vars : int) : bool Array.t Array.t =
  let num_combos = int_of_float (float_of_int num_vars ** 2.) in
  Array.make_matrix num_combos num_vars false

let rec eval (e : exp) (vars : bool Array.t) : bool =
  match e with
  | Var v -> vars.(v - 1)
  | Not e -> not (eval e vars)
  | And (e1, e2) -> (eval e1 vars) && (eval e2 vars)
  | Or (e1, e2) -> (eval e1 vars) || (eval e2 vars)

(* exps for tests *)
let not_gate = Not (Var 1)
let and_gate = And (Var 1, Var 2)
let nand_gate = Not (And (Var 1, Var 2))
let or_gate = Or (Var 1, Var 2)
let nor_gate = Not (Or (Var 1, Var 2))
let xor_gate = Or (And (Not (Var 1), Var 2), And (Var 1, Not (Var 2)))
let xnor_gate = Not (Or (And (Not (Var 1), Var 2), And (Var 1, Not (Var 2))))

(* utility functions for tests *)
let print_result result =
  if result then print_string "1\n"
  else print_string "0\n"

let expect result =
  if not result then failwith "Error"

let expect_not result =
  if result then failwith "Error"

  (* testing individual boolean functions *)
let test_not_gate () =
  expect_not (eval not_gate [|true|]);
  expect (eval not_gate [|false|])

let test_and_gate () =
  expect_not (eval and_gate [|false; false|]);
  expect_not (eval and_gate [|true; false|]);
  expect_not (eval and_gate [|false; true|]);
  expect (eval and_gate [|true; true|])

let test_nand_gate () =
  expect (eval nand_gate [|false; false|]);
  expect (eval nand_gate [|true; false|]);
  expect (eval nand_gate [|false; true|]);
  expect_not (eval nand_gate [|true; true|])

let test_or_gate () =
  expect_not (eval or_gate [|false; false|]);
  expect (eval or_gate [|true; false|]);
  expect (eval or_gate [|false; true|]);
  expect (eval or_gate [|true; true|])

let test_nor_gate () =
  expect (eval nor_gate [|false; false|]);
  expect_not (eval nor_gate [|true; false|]);
  expect_not (eval nor_gate [|false; true|]);
  expect_not (eval nor_gate [|true; true|])

let test_xor_gate () =
  expect_not (eval xor_gate [|false; false|]);
  expect (eval xor_gate [|true; false|]);
  expect (eval xor_gate [|false; true|]);
  expect_not (eval xor_gate [|true; true|])

let test_xnor_gate () =
  expect (eval xnor_gate [|false; false|]);
  expect_not (eval xnor_gate [|true; false|]);
  expect_not (eval xnor_gate [|false; true|]);
  expect (eval xnor_gate [|true; true|])

(* test *)
let _ =
  test_not_gate ();
  test_and_gate ();
  test_nand_gate ();
  test_or_gate ();
  test_nor_gate ();
  test_xor_gate ();
  test_xnor_gate ();
