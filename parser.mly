%{
  open Printf
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

%left PLUS MINUS
%left MULTIPLY DIVIDE
%left NEGATION
%left NEGATIVE
%right EXP

%start input
%type <unit> input

%%
  input:  /* empty */ { }
    | STRING_LITERAL {}
  ;
%%