%{
  open Printf
  open Interface

  let parse_error s = (* Called by the parser function on error *)
    print_endline s;
    flush stdout
%}

%token <string> STRING_LITERAL
%token <bool> BOOLEAN_LITERAL
%token DISJUNCTION_KEYWORD
%token CONJUCTION_KEYWORD
%token <string> RELATIONAL_OPERATOR
%token PLUS MINUS MULTIPLY DIVIDE EXP
%token NEGATION
%token NEGATIVE
%token <string> CNAME
%token <int> INTEGER_LITERAL
%token <string> COMMENT
%token NEWLINE
%token WHITESPACE
%token <string> ID

%token OPEN_CURLY_BRACKET_KEYWORD
%token CLOSE_CURLY_BRACKET_KEYWORD
%token CLASS_KEYWORD
%token VOID_MAIN_KEYWORD
%token OPEN_BRACKET_KEYWORD
%token CLOSE_BRACKET_KEYWORD
%token COMMA_KEYWORD
%token EOF

%token IF_KEYWORD
%token ELSE_KEYWORD
%token WHILE_KEYWORD
%token READLN_KEYWORD
%token PRINTLN_KEYWORD
%token RETURN_KEYWORD
%token SEMICOLLON_KEYWORD
%token ASSIGNMENT_KEYWORD
%token DOT_KEYWORD
%token THIS_KEYWORD;
%token NEW_KEYWORD;
%token NULL_KEYWORD;

%left PLUS MINUS
%left MULTIPLY DIVIDE
%left NEGATION
%left NEGATIVE
%right EXP

%start program
%type <Interface.jlite_program> program

%%
program:  
  main_class class_decls { 
    printf "# program\n";
    ($1, $2);
  }
;

class_decls:
  { 
    printf "# empty class_decls\n";
    [];
  }
  | class_decls class_decl { 
    printf "# class_decls\n";
    $2 :: $1;
  }

main_class:
  CLASS_KEYWORD CNAME OPEN_CURLY_BRACKET_KEYWORD
  VOID_MAIN_KEYWORD OPEN_BRACKET_KEYWORD fml_list CLOSE_BRACKET_KEYWORD 
  md_body CLOSE_CURLY_BRACKET_KEYWORD 
  { 
    printf "# main_class\n";
    let (localvars_l, stmts_l) = $8
    and my_rnd = string_of_int (Random.int 100000000) in
      let res : md_decl = {
        jliteid = SimpleVarId my_rnd;
        ir3id = SimpleVarId my_rnd;
        rettype = VoidT;
        params = $6;
        localvars = localvars_l;
        stmts = stmts_l;
      } in ($2, res)
  }
;

class_decl:
  CLASS_KEYWORD CNAME OPEN_CURLY_BRACKET_KEYWORD 
  var_decls_k md_decls_k CLOSE_CURLY_BRACKET_KEYWORD 
  { 
    printf "# class_decl\n";
    ($2, $4, $5);
  }
;

var_decls_k:
  { 
    printf "# empty var_decls_k\n";
    []
  }
  | var_decls_k var_decl_k { 
    printf "# var_decls_k\n";
    $2 :: $1;
  }
;

var_decl_k:
  type_k ID { 
    printf "#var_decl_k\n";
    ($1, SimpleVarId $2);
  }
;

md_decls_k:
  { 
    printf "# empty md_decls_k\n";
    [];
  }
  | md_decls_k md_decl_k { 
    printf "# md_decls_k\n"; 
    $2 :: $1;
  }
;

md_decl_k:
  type_k ID OPEN_BRACKET_KEYWORD fml_list CLOSE_BRACKET_KEYWORD md_body { 
    printf "# md_decl_k\n"; 

    let (localvars_l, stmts_l) = $6
    and my_rnd = string_of_int (Random.int 100000000) in
      let res : md_decl = {
        jliteid = SimpleVarId my_rnd;
        ir3id = SimpleVarId my_rnd;
        rettype = $1;
        params = $4;
        localvars = localvars_l;
        stmts = stmts_l;
      } in res
  }
;

fml_list:
  { 
    printf "# empty fml_list\n";
    [];
  }
  | type_k ID fml_rests { 
    printf "fml_list\n";
    ($1, SimpleVarId $2) :: $3;
  }
;

fml_rests:
  { 
    printf "# empty fml_rests\n";
    [];
  }
  | fml_rests fml_rest { 
    printf "# fml_rests\n"; 
    $2 :: $1;
  }
;

fml_rest:
  COMMA_KEYWORD type_k ID { 
    printf "# fml_rest\n"; 
    ($2, SimpleVarId $3);
    (* assume 1 is local variable *)
  }
;

type_k:
  CNAME {
    printf "# type\n"; 
    match $1 with
      "Int" -> IntT
    | "Bool" -> BoolT
    | "String" -> StringT
    | "Object" -> ObjectT $1
    | "Void" -> VoidT
    | _ -> Unknown 
  }
;

md_body:
  OPEN_CURLY_BRACKET_KEYWORD var_decls_k stmt stmts CLOSE_CURLY_BRACKET_KEYWORD { 
    printf "# md_body\n"; 
    (*var_decls list, stmts list*)
    ($2, $3::$4)
  }
;

stmts:
  { 
    printf "# empty stmts\n";
    []
  }
  | stmts stmt { 
    printf "# stmts\n"; 
    $2 :: $1;
  }
;

stmt:
  IF_KEYWORD OPEN_BRACKET_KEYWORD exp CLOSE_BRACKET_KEYWORD OPEN_CURLY_BRACKET_KEYWORD stmt stmts
    CLOSE_CURLY_BRACKET_KEYWORD ELSE_KEYWORD OPEN_CURLY_BRACKET_KEYWORD stmt stmts CLOSE_CURLY_BRACKET_KEYWORD { 
      printf "# stmt\n"; 
      IfStmt ($3, ($6 :: $7), ($11 :: $12));
    }
  | WHILE_KEYWORD OPEN_BRACKET_KEYWORD exp CLOSE_BRACKET_KEYWORD OPEN_CURLY_BRACKET_KEYWORD stmts CLOSE_CURLY_BRACKET_KEYWORD { 
    printf "# stmt\n";
    WhileStmt ($3, $6);
  }
  | READLN_KEYWORD OPEN_BRACKET_KEYWORD ID CLOSE_BRACKET_KEYWORD SEMICOLLON_KEYWORD { 
    printf "# stmt\n"; 
    ReadStmt (SimpleVarId $3);
  }
  | PRINTLN_KEYWORD OPEN_BRACKET_KEYWORD exp CLOSE_BRACKET_KEYWORD SEMICOLLON_KEYWORD { 
    printf "# stmt\n"; 
    PrintStmt $3;
  }
  | ID ASSIGNMENT_KEYWORD exp SEMICOLLON_KEYWORD { 
    printf "# stmt\n"; 
    AssignStmt (SimpleVarId $1, $3);
  }
  | atom DOT_KEYWORD ID ASSIGNMENT_KEYWORD exp SEMICOLLON_KEYWORD { 
    printf "# stmt\n"; 
    AssignFieldStmt (FieldAccess ($1, SimpleVarId $3), $5);
  }
  | atom OPEN_BRACKET_KEYWORD exp_list CLOSE_BRACKET_KEYWORD { 
    printf "# stmt\n"; 
    MdCallStmt (MdCall ($1, $3));
  }
  | RETURN_KEYWORD exp SEMICOLLON_KEYWORD { 
    printf "# stmt\n"; 
    ReturnStmt ($2);
  }
  | RETURN_KEYWORD SEMICOLLON_KEYWORD { 
    printf "# stmt\n"; 
    ReturnVoidStmt
  }
;

exp:
  b_exp { 
    printf "exp\n"; 
    $1;
  }
  | a_exp { 
    printf "exp\n";
    $1;
  }
  | s_exp { 
    printf "exp\n";
    $1;
  }
;

b_exp:
  b_exp DISJUNCTION_KEYWORD conj {
    printf "b_exp\n";
    BinaryExp ((BooleanOp "||"), $1, $3);
  }
  | conj {
    printf "b_exp\n";
    $1;
  }
;

conj:
  conj CONJUCTION_KEYWORD r_exp {
    printf "conj\n";
    BinaryExp ((BooleanOp "&&"), $1, $3);
  }
  | r_exp {
    printf "conj\n";
    $1;
  }
;

r_exp:
  a_exp b_op a_exp {
    printf "r_exp\n";
    BinaryExp ($2, $1, $3);
  }
  | b_grd {
    printf "r_exp\n";
    $1;
  }
;

b_op: 
  RELATIONAL_OPERATOR {
    printf "b_op\n"; 
    RelationalOp $1;
  }
;

b_grd:
  NEGATION b_grd { 
    printf "b_grd\n";
    UnaryExp ((UnaryOp "!"), $2);
  }
  | BOOLEAN_LITERAL { 
    printf "b_grd\n";
    BoolLiteral $1;
  }
  | atom { 
    printf "b_grd\n";
    $1;
  }
;

a_exp:
  a_exp PLUS term {
    printf "a_exp\n";
    BinaryExp ((AritmeticOp "+"), $1, $3);
  }
  | a_exp MINUS term {
    printf "a_exp\n";
    BinaryExp ((AritmeticOp "-"), $1, $3); 
  }
  | term {
    printf "a_exp\n";
    $1;
  }
;

term:
  term MULTIPLY ftr {
    printf "term\n";
    BinaryExp ((AritmeticOp "*"), $1, $3);
  }
  | term DIVIDE ftr {
    printf "term\n";
    BinaryExp ((AritmeticOp "/"), $1, $3);
  }
  | ftr {
    printf "term\n";
    $1;
  }
;

ftr:
  INTEGER_LITERAL {
    printf "ftr\n";
    IntLiteral $1;
  }
  | NEGATIVE ftr {
    printf "ftr\n";
    UnaryExp ((UnaryOp "-"), $2);
  }
  | atom {
    printf "ftr\n";
    $1;
  }
;

s_exp:
  STRING_LITERAL {
    printf "s_exp\n";
    StringLiteral $1;
  }
  | atom {
    printf "s_exp\n";
    $1;
  }
;

atom:
  atom DOT_KEYWORD ID {
    printf "atom\n";
    FieldAccess ($1, (SimpleVarId $3));
  }
  | atom OPEN_BRACKET_KEYWORD exp_list CLOSE_BRACKET_KEYWORD {
    printf "atom\n";
    MdCall ($1, $3);
  }
  | THIS_KEYWORD {
    printf "atom\n";
    ThisWord;
  }
  | ID {
    printf "atom\n";
    Var (SimpleVarId $1);
  }
  | NEW_KEYWORD CNAME {
    printf "atom\n";
    ObjectCreate $2;
  }
  | OPEN_BRACKET_KEYWORD exp CLOSE_BRACKET_KEYWORD {
    printf "atom\n";
    $2;
  }
  | NULL_KEYWORD {
    printf "atom\n";
    NullWord;
  }
;

exp_list:
  { 
    printf "empty exp_list\n";
    [];
  }
  | exp exp_rests { 
    printf "exp_list\n";
    $1 :: $2;
  }
;

exp_rests:
  { 
    printf "empty exp_rests\n";
    [];
  }
  | exp_rests exp_rest { 
    printf "exp_rests\n";
    $2 :: $1;
  }
;

exp_rest: COMMA_KEYWORD exp { 
    printf "exp_rest\n";
    $2;
  }
;

%%