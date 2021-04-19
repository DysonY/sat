open Exp

module IntSet =
  Set.Make(struct
    type t = int
    let compare = Stdlib.compare
  end)

(* Generate empty array of dimensions 2^n by n *)
let gen_empty num_vars num_combos : bool array array =
  Array.make_matrix num_combos num_vars false

(* Populate empty array w/ all configurations of inputs *)
let populate table num_vars num_combos : bool array array =
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
let count_vars (e : exp) : int =
  let found = ref IntSet.empty in
  let rec search (e : exp) : int =
    match e with
    | Var v ->
      if (IntSet.mem v !found) then 0
      else begin found := IntSet.add v !found; 1 end
    | Not e -> search e
    | And (e1, e2) | Or (e1, e2) -> (search e1) + (search e2)
  in
  search e

(* Find a configuration for a particular expression *)
let find_config (e : exp) : bool array =
  let num_vars = count_vars e in
  let combos = gen_combos num_vars in
  let temp = ref (Array.make num_vars false) in
  try
    for i = 0 to Array.length combos - 1 do
      temp := combos.(i);
      if eval e !temp then raise Exit
    done;
    print_string "No configuration found.\n";
    [||]
  with
  | Exit ->
    !temp
