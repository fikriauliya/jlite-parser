# export OCAMLRUNPARAM='p'
export OCAMLRUNPARAM=''

ocamlyacc -v parser.mly
ocamlc -c parser.mli

ocamllex lexer.mll
ocamlc -c lexer.ml

ocamlc -c parser.ml
ocamlc -c main.ml

ocamlc -o as1 lexer.cmo parser.cmo main.cmo