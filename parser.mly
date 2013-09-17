%{
  open Printf


  let parse_error s = (* Called by the parser function on error *)
    print_endline s;
    flush stdout

  type class_name = string
  type jlite_type =
    | IntT
    | BoolT
    | StringT
    | ObjectT of class_name
    | VoidT
    (* Reserved for type checking. Please do not instantiate during parsing *)
    | Unknown

  type jlite_op =
    | BooleanOp of string
    | RelationalOp of string
    | AritmeticOp of string
    | UnaryOp of string
  type var_id =
    | SimpleVarId of string
    (* Reserved for type checking. Please do not instantiate during parsing *)
    | TypedVarId of typed_var_id
    (* A jlite variable id annotated with its type and scope *)
    (* The scope can be class, parameter or local variable *)
  and typed_var_id = string * jlite_type * int
  type jlite_exp =
    | UnaryExp of jlite_op * jlite_exp
    | BinaryExp of jlite_op * jlite_exp * jlite_exp
    | FieldAccess of jlite_exp * var_id
    | ObjectCreate of class_name
    | MdCall of jlite_exp * (jlite_exp list)
    | BoolLiteral of bool
    | IntLiteral of int
    | StringLiteral of string
    | ThisWord
    | NullWord
    | Var of var_id
    (* Reserved for type checking. Please do not instantiate during parsing *)
    | TypedExp of jlite_exp * jlite_type
  and jlite_stmt =
    | IfStmt of jlite_exp * (jlite_stmt list) * (jlite_stmt list)
    | WhileStmt of jlite_exp * (jlite_stmt list)
    | ReadStmt of var_id
    | PrintStmt of jlite_exp
    | AssignStmt of var_id * jlite_exp
    | AssignFieldStmt of jlite_exp * jlite_exp
    | MdCallStmt of jlite_exp
    | ReturnStmt of jlite_exp
    | ReturnVoidStmt

  type var_decl = jlite_type * var_id
  and md_decl =
    {
    (* ID of the method in the original Jlite Program *)
    jliteid: var_id;
    (* Unique ID of the method in the Jlite Program *)
    (* Mutable value intially equal to jliteid *)
    mutable ir3id: var_id ;
    rettype: jlite_type;
    params:(var_decl list);
    localvars:(var_decl list);
    stmts:(jlite_stmt list)
  }
  and class_decl = class_name * (var_decl list) * (md_decl list)
  and class_main = class_name * md_decl
  and jlite_program = class_main * (class_decl list)
%}

%token <string> STRING_LITERAL
%token <bool> BOOLEAN_LITERAL
%token <string> BOOLEAN_OPERATOR
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
%token DISJUNCTION_KEYWORD
%token CONJUCTION_KEYWORD
%token THIS_KEYWORD;
%token NEW_KEYWORD;
%token NULL_KEYWORD;

%left PLUS MINUS
%left MULTIPLY DIVIDE
%left NEGATION
%left NEGATIVE
%right EXP

%start program
%type <unit> program

%%
program:  
  { }
  | main_class class_decls { printf "# program\n";}
;

class_decls:
  { printf "# empty class_decls\n" }
  | class_decls class_decl { printf "# class_decls\n"; }

main_class:
  CLASS_KEYWORD CNAME OPEN_CURLY_BRACKET_KEYWORD
  VOID_MAIN_KEYWORD OPEN_BRACKET_KEYWORD fml_list CLOSE_BRACKET_KEYWORD 
  md_body CLOSE_CURLY_BRACKET_KEYWORD 
    { printf "# main_class\n" }
;

class_decl:
  CLASS_KEYWORD CNAME OPEN_CURLY_BRACKET_KEYWORD 
  var_decls_k md_decls_k CLOSE_CURLY_BRACKET_KEYWORD 
    { printf "# class_decl\n" }
;

var_decls_k:
  { printf "# empty var_decls_k\n" }
  | var_decls_k var_decl_k { printf "# var_decls_k\n"; }
;

var_decl_k:
  type_k ID { printf "#var_decl_k\n"}
;

md_decls_k:
  { printf "# empty md_decls_k\n" }
  | md_decls_k md_decl_k { printf "# md_decls_k\n"; }
;

md_decl_k:
  type_k ID OPEN_BRACKET_KEYWORD fml_list CLOSE_BRACKET_KEYWORD md_body { printf "# md_decl_k\n"; }
;

fml_list:
  { printf "# empty fml_list\n" }
  | type_k ID fml_rests { printf "fml_list\n" }
;

fml_rests:
  { printf "# empty fml_rests\n" }
  | fml_rests fml_rest { printf "# fml_rests\n"; }
;

fml_rest:
  COMMA_KEYWORD type_k ID { printf "# fml_rest\n"; }
;

type_k:
  CNAME {printf "# type\n";}
;

md_body:
  OPEN_CURLY_BRACKET_KEYWORD var_decls_k stmt stmts CLOSE_CURLY_BRACKET_KEYWORD { printf "# md_body\n"; }
;

stmts:
  { printf "# empty stmts\n" }
  | stmts stmt { printf "# stmts\n"; }
;

stmt:
  IF_KEYWORD OPEN_BRACKET_KEYWORD exp CLOSE_BRACKET_KEYWORD OPEN_CURLY_BRACKET_KEYWORD stmt stmts
    CLOSE_CURLY_BRACKET_KEYWORD ELSE_KEYWORD OPEN_CURLY_BRACKET_KEYWORD stmt stmts CLOSE_CURLY_BRACKET_KEYWORD { printf "# stmt\n"; }
  | WHILE_KEYWORD OPEN_BRACKET_KEYWORD exp CLOSE_BRACKET_KEYWORD OPEN_CURLY_BRACKET_KEYWORD stmts CLOSE_CURLY_BRACKET_KEYWORD { printf "# stmt\n"; }
  | READLN_KEYWORD OPEN_BRACKET_KEYWORD ID CLOSE_BRACKET_KEYWORD SEMICOLLON_KEYWORD { printf "# stmt\n"; }
  | PRINTLN_KEYWORD OPEN_BRACKET_KEYWORD exp CLOSE_BRACKET_KEYWORD SEMICOLLON_KEYWORD { printf "# stmt\n"; }
  | ID ASSIGNMENT_KEYWORD exp SEMICOLLON_KEYWORD { printf "# stmt\n"; }
  | atom DOT_KEYWORD ID ASSIGNMENT_KEYWORD exp SEMICOLLON_KEYWORD { printf "# stmt\n"; }
  | atom OPEN_BRACKET_KEYWORD exp_list CLOSE_BRACKET_KEYWORD { printf "# stmt\n"; }
  | RETURN_KEYWORD exp SEMICOLLON_KEYWORD { printf "# stmt\n"; }
  | RETURN_KEYWORD SEMICOLLON_KEYWORD { printf "# stmt\n"; }
;

exp:
  b_exp { printf "exp\n"}
  | a_exp { printf "exp\n"}
  | s_exp { printf "exp\n"}
;

b_exp:
  b_exp DISJUNCTION_KEYWORD conj {printf "b_exp\n"}
  | conj {printf "b_exp\n"}
;

conj:
  conj CONJUCTION_KEYWORD r_exp {printf "conj\n"}
  | r_exp {printf "conj\n"}
;

r_exp:
  a_exp b_op a_exp {printf "r_exp\n"}
  | b_grd {printf "r_exp\n"}
;

b_op: 
  RELATIONAL_OPERATOR {printf "b_op\n"}
;

b_grd:
  NEGATION b_grd { printf "b_grd\n"}
  | BOOLEAN_LITERAL { printf "b_grd\n"}
  | atom { printf "b_grd\n"}
;

a_exp:
  a_exp PLUS term {printf "a_exp\n"}
  | a_exp MINUS term {printf "a_exp\n"}
  | term {printf "a_exp\n"}
;

term:
  term MULTIPLY ftr {printf "term\n"}
  | term DIVIDE ftr {printf "term\n"}
  | ftr {printf "term\n"}
;

ftr:
  INTEGER_LITERAL {printf "ftr\n"}
  | NEGATIVE ftr {printf "ftr\n"}
  | atom {printf "ftr\n"}
;

s_exp:
  STRING_LITERAL {printf "s_exp\n"}
  | atom {printf "s_exp\n"}
;

atom:
  atom DOT_KEYWORD ID {printf "atom\n"}
  | atom OPEN_BRACKET_KEYWORD exp_list CLOSE_BRACKET_KEYWORD {printf "atom\n"}
  | THIS_KEYWORD {printf "atom\n"}
  | ID {printf "atom\n"}
  | NEW_KEYWORD CNAME {printf "atom\n"}
  | OPEN_BRACKET_KEYWORD exp CLOSE_BRACKET_KEYWORD {printf "atom\n"}
  | NULL_KEYWORD {printf "atom\n"}
;

exp_list:
  { printf "empty exp_list\n"}
  |exp exp_rests { printf "exp_list\n"}
;

exp_rests:
  { printf "empty exp_rests\n"}
  | exp_rests exp_rest { printf "exp_rests\n"}
;

exp_rest: COMMA_KEYWORD exp { printf "exp_rest\n"}
;

%%