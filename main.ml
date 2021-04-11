open Exp

(* Generate empty array of dimensions 2^n by n *)
let gen_empty num_vars num_combos : bool array array =
  Array.make_matrix num_combos num_vars false

(* Populate empty array w/ all configurations of inputs *)
let populate table num_vars num_combos : bool array array=
  let fill = ref false in
  let skip = ref 1 in
  let temp = ref 0 in
  for col = (num_vars - 1) downto 0 do
    for row = 0 to (num_combos - 1) do
      if !temp = !skip then
        begin
          temp := 0;
          fill := not !fill;
        end;
      if !fill then table.(row).(col) <- true;
      temp := !temp + 1;
    done
    ; skip := !skip * 2
  done
  ; table

(* Generate a truth table with 2^n configurations *)
let gen_combos (num_vars : int) : bool array array =
  let num_combos = int_of_float (2. ** float_of_int num_vars) in
  let empty = gen_empty num_vars num_combos in
  populate empty num_vars num_combos

(* Evaluate a boolean expression for a given configuration of inputs *)
let rec eval (e : exp) (vars : bool array) : bool =
  match e with
  | Var v -> vars.(v - 1)
  | Not e -> not (eval e vars)
  | And (e1, e2) -> (eval e1 vars) && (eval e2 vars)
  | Or (e1, e2) -> (eval e1 vars) || (eval e2 vars)

(* Count the number of variables in a given exp *)
let rec count_vars (e : exp) : int =
  match e with
  | Var v -> 1
  | Not e -> count_vars e
  | And (e1, e2) | Or (e1, e2) -> (count_vars e1) + (count_vars e2)

let find_config (e : exp) =
  let num_vars = count_vars e in
  let combos = gen_combos num_vars in
  let temp = ref (Array.make num_vars false) in
  try
    for i = 0 to Array.length combos - 1 do
      temp := combos.(i);
      if eval e !temp then raise Exit
    done
    ; print_string "No configuration found."
  with
  | Exit -> print_string "Configuration found!"

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
