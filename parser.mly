%{
  open Printf


  let parse_error s = (* Called by the parser function on error *)
    print_endline s;
    flush stdout
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
  var_decls md_decls CLOSE_CURLY_BRACKET_KEYWORD 
    { printf "# class_decl\n" }
;

var_decls:
  { printf "# empty var_decls\n" }
  | var_decls var_decl { printf "# var_decls\n"; }
;

var_decl:
  type_k ID { printf "#var_decl\n"}
;

md_decls:
  { printf "# empty md_decls\n" }
  | md_decls md_decl { printf "# md_decls\n"; }
;

md_decl:
  type_k ID OPEN_BRACKET_KEYWORD fml_list CLOSE_BRACKET_KEYWORD md_body { printf "# md_decl\n"; }
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
  OPEN_CURLY_BRACKET_KEYWORD var_decls CLOSE_CURLY_BRACKET_KEYWORD { printf "# md_body\n"; }
;
%%