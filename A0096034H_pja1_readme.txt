1) How to run?

Compile:
./make

Run:
./as1 < program_1
./as1 < program_2
./as1 < program_3

2) Note:
I left some debugging message printed.

3) Convertion on the given grammar to avoid shift/reduce conflict:

class_decl -> class <CNAME> { class_body } 
class_body
  -> empty
  | <type_k> <ID> ; <class_body>
  | <type_k> <ID> { <fml_list> } <md_body> <class_body>


4) Convertion on the given grammar to avoid 6 reduce/reduce conflicts:

exp -> 
  b_exp 
  | a_exp 
  | s_exp 
  | atom 


b_exp ->
  b_exp || conj 
  | b_exp || atom
  | conj 


conj ->
  conj && r_exp 
  | conj && atom
  | r_exp 


r_exp ->
  a_exp b_op a_exp 
  | a_exp b_op atom 
  | atom b_op a_exp 
  | atom b_op atom 
  | b_grd 


b_op -> 
  RELATIONAL_OPERATOR 


b_grd ->
  !b_grd 
  | BOOLEAN_LITERAL
  | !atom 


a_exp ->
  a_exp + term 
  | atom + term
  | a_exp + atom 
  | atom + atom 
  | a_exp - term 
  | atom - term 
  | a_exp - atom 
  | atom - atom 
  | term 


term ->
  term * ftr 
  | term * atom
  | atom * ftr 
  | atom * atom
  | term / ftr 
  | term / atom 
  | atom / ftr 
  | atom / atom
  | ftr 

ftr ->
  INTEGER_LITERAL 
  | - ftr 
  | - atom


s_exp ->
  STRING_LITERAL 