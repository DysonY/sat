compile:
	ocamlc exp.ml -o exp
	ocamlc token.ml -o token
	ocamlc lex.ml -o lex
	ocamlc parse.ml -o parse
	ocamlc eval.ml -o eval
	ocamlc exp.cmo token.cmo lex.cmo parse.cmo eval.cmo main.ml -o main
