export OCAMLRUNPARAM=''
# export OCAMLRUNPARAM=''

./clean.sh

ocamlc -c jlite_structs.ml
ocamlc -c displayfunc.ml

ocamlyacc -v jlite_parser.mly
ocamlc -c jlite_parser.mli

ocamllex jlite_lexer.mll
ocamlc -c jlite_lexer.ml

ocamlc -c jlite_parser.ml
ocamlc -c main.ml

ocamlc -o as1 jlite_structs.cmo jlite_lexer.cmo jlite_parser.cmo displayfunc.cmo main.cmo