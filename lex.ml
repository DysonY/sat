open Token

(* Explode a string into a list *)
let explode (str : string) : char list =
  let rec loop index output =
    if index < 0 then output
    else loop (index - 1) (str.[index] :: output)
  in
  loop (String.length str - 1) []

(* Join a list of int characters into a NUM token *)
let collapse (int_chars : char list) : token list =
  if int_chars = [] then []
  else
    let rec loop chars int_str =
      match chars with
      | [] -> int_of_string int_str
      | c :: cs when Char.code c > 47 && Char.code c < 58 ->
        loop cs (int_str ^ (String.make 1 c))
      | c :: _ -> failwith ("invalid character: " ^ (String.make 1 c))
    in
    [NUM (loop int_chars "")]

(* Generate a list of tokens from an input string *)
let tokenize (s : string) : token list =
  let stream = explode s in
  let rec loop chars tokens int_chars =
    match chars with
    | [] -> tokens @ (collapse int_chars)
    | '(' :: cs -> loop cs (tokens @ (collapse int_chars) @ [LPAREN]) []
    | ')' :: cs -> loop cs (tokens @ (collapse int_chars) @ [RPAREN]) []
    | '~' :: cs -> loop cs (tokens @ (collapse int_chars) @ [NOT])    []
    | '&' :: cs -> loop cs (tokens @ (collapse int_chars) @ [AND])    []
    | '|' :: cs -> loop cs (tokens @ (collapse int_chars) @ [OR])     []
    | '\n' :: cs | '\t' :: cs | ' ' :: cs ->
      loop cs (tokens @ (collapse int_chars)) []
    | c :: cs when Char.code c > 47 && Char.code c < 58 ->
      loop cs tokens (int_chars @ [c])
    | c :: _ -> failwith ("invalid character: " ^ (String.make 1 c))
  in
  loop stream [] []
