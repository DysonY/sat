%{
  type exp =
  | Num of int
  | Block of exp
  | And of exp * exp
  | Or of exp * exp
  | Not of exp
%}

%token<int> NUM
%token LPAREN RPAREN NOT AND OR

%start exp
%type<exp> exp

%left NOT AND OR

%%
exp:
| NUM			{ Num $1 }

| LPAREN exp RPAREN	{ Block $2 }

| exp AND exp		{ And ($1, $3) }

| exp OR exp		{ Or ($1, $3) }

| NOT exp		{ Not $2 }
%%
