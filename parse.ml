open Exp
open Token

(* Helper function for expect *)
let require (target : token) (tokens : token list) =
  match tokens with
  | t :: ts when t = target -> ts
  | _ -> failwith "Parse require failed."

(* Given LPAREN, search for RPAREN *)
let expect parse_fun (target : token) (tokens : token list) =
  let (v, ts) = parse_fun tokens in
  let ts = require target ts in
  (v, ts)

(* Scan and/or block *)
let and_or curr_fn next_fn (tokens : token list) =
  let rec parse_temp (e1 : exp) (ts : token list) =
    match ts with
    | [] -> (e1, ts)
    | t :: ts ->
      match curr_fn t with
      | None -> (e1, t :: ts)
      | Some fn ->
        let (e2, ts) = next_fn ts in
        parse_temp (fn e1 e2) ts
  in
  let (e1, ts) = next_fn tokens in
  parse_temp e1 ts

(* Helpers for and_or *)
let and_fn = Some (function a -> function b -> And (a, b))
let or_fn = Some (function a -> function b -> Or (a, b))

(* Parse NUM, NOT, LPAREN *)
let rec parse_atom (tokens : token list) : (exp * token list) =
  match tokens with
  | NUM i :: ts -> (Var i, ts)
  | NOT :: ts ->
    let (e, ts) = parse_atom ts in
    (Not e, ts)
  | LPAREN :: ts -> expect parse RPAREN ts
  | _ -> failwith "Parse atom failed."

(* Parse AND, OR *)
and parse_term (tokens : token list) : (exp * token list) =
  let f (t : token) =
    match t with
    | AND -> and_fn
    | OR -> or_fn
    | _ -> None
  in
  and_or f parse_atom tokens

(* Main parsing function *)
and parse (tokens : token list) : (exp * token list) =
  parse_term tokens

(* TESTS FOR PARSE *)
(*
let tests = [ [NUM 1; AND; NUM 2]
            ; [NUM 1; OR; NUM 2]
            ; [NOT; NUM 1]
            ; [NUM 1]
            ; [NOT; LPAREN; NUM 1; AND; NUM 2; RPAREN]
            ; [NUM 1; AND; NOT; LPAREN; NUM 2; OR; NUM 3; RPAREN]
            ; [LPAREN; RPAREN] (* fail *)
            ; [NUM 1; AND; LPAREN; NUM 2] (* fail *) ]

let rec test ts =
  match ts with
  | [] -> ()
  | t :: ts ->
    let _ = parse t in
    test ts

let _ = test tests
*)
