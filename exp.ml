type exp =
  | Var of int
  | Not of exp
  | And of exp * exp
  | Or of exp * exp
