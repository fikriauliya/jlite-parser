export OCAMLRUNPARAM=''
# export OCAMLRUNPARAM=''

ocamlc -c interface.ml
ocamlc -c displayfunc.ml

ocamlyacc -v parser.mly
ocamlc -c parser.mli

ocamllex lexer.mll
ocamlc -c lexer.ml

ocamlc -c parser.ml
ocamlc -c main.ml

ocamlc -o as1 interface.cmo lexer.cmo parser.cmo displayfunc.cmo main.cmo