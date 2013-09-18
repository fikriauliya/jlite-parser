{
  open Printf
  open Parser
}

let digit = ['0'-'9']
let hex = ['0'-'9' 'A'-'F']
let underscore = '_'
let letter = ['a'-'z' 'A'-'Z']
let escaped_special_charater = ['\\' '\n' '\r' '\t' '\b']
let normal_character = [^ '\\' '"' '\n' '\r']
let single_line_comment_start = "//"
let multi_line_comment_start = "/*"
let multi_line_comment_end = "*/"

rule token = parse
  | single_line_comment_start [^ '\n']* '\n' as comment {
      printf "single line comment\n";
      token lexbuf;
    }
  | multi_line_comment_start {
      printf "multi line comment start\n";
      comment_token "" lexbuf
    }

  (* Omit white spaces *)
  | [' ' '\t']  { 
      (* printf "white space\n"; *)
      token lexbuf;
    }
  | '\n'  { 
      (* printf "new line\n"; *)
      token lexbuf;
    }
    
  | '{' {
      printf "keyword: {\n";
      OPEN_CURLY_BRACKET_KEYWORD;
    }
  | '}' {
      printf "keyword: }\n";
      CLOSE_CURLY_BRACKET_KEYWORD;
    }
  | "class" {
      printf "keyword: class\n";
      CLASS_KEYWORD;
    }
  | "void main" {
      printf "keyword: void main\n";
      VOID_MAIN_KEYWORD;
    }
  | '(' {
      printf "keyword: (\n";
      OPEN_BRACKET_KEYWORD;
    }
  | ')' {
      printf "keyword: )\n";
      CLOSE_BRACKET_KEYWORD;
    }
  | ',' {
      printf "keyword: ,\n";
      COMMA_KEYWORD;
    }
  | "if" {
      printf "keyword: if\n";
      IF_KEYWORD;
    }
  | "else" {
      printf "keyword: else\n";
      ELSE_KEYWORD;
    }
  | "while" {
      printf "keyword: while\n";
      WHILE_KEYWORD;
    }
  | "readln" {
      printf "keyword: readln\n";
      READLN_KEYWORD;
    }
  | "println" {
      printf "keyword: println\n";
      PRINTLN_KEYWORD;
    }
  | "return" {
      printf "keyword: return\n";
      RETURN_KEYWORD;
    }
  | ';' {
      printf "keyword: ;\n";
      SEMICOLLON_KEYWORD;
    }
  | '=' {
      printf "keyword: =\n";
      ASSIGNMENT_KEYWORD; 
    }
  | '.' {
      printf "keyword: .\n";
      DOT_KEYWORD;  
    }

  | "||" {
      printf "keyword: ||\n";
      DISJUNCTION_KEYWORD;
    }
  | "&&" {
      printf "keyword: &&\n";
      CONJUCTION_KEYWORD;
    }
  | "this" {
      printf "keyword: this\n";
      THIS_KEYWORD;
    }
  | "new" {
      printf "keyword: new\n";
      NEW_KEYWORD;
    }
  | "NULL" {
      printf "keyword: NULL\n";
      NULL_KEYWORD;
    }

  | "\"" (escaped_special_charater | ("\\0" digit digit) | ("\\x" hex hex) | normal_character)* "\"" as string_literal {
      printf "string literal: %s\n" string_literal;
      STRING_LITERAL (string_literal)
    }
  | "true" | "false" as boolean_literal {
      printf "boolean literal: %b\n" (bool_of_string boolean_literal);
      BOOLEAN_LITERAL (bool_of_string boolean_literal);
    }
  | ">=" | "<=" | '>' | '<' | "!=" | "==" as operator {
      printf "relational operator: %s\n" operator;
      RELATIONAL_OPERATOR (operator);
    }
  | '+' { 
      printf "plus\n";
      PLUS;
    }
  | '-' { 
      printf "minus\n";
      MINUS;
    }
  | '*' {
      printf "multiply\n";
      MULTIPLY;
    }
  | '/' {
      printf "divide\n";
      DIVIDE;
    }
  | '^' {
      printf "exp\n";
      EXP;
    }
  | '!' {
      printf "negation operator\n";
      NEGATION;
    }
  | '-' {
      printf "negative operator\n";
      NEGATIVE;
    }
  | ['a'-'z'] (letter | digit | underscore)* as id {
      printf "id: %s\n" id;
      ID (id);
    }
  | ['A'-'Z'] (letter | digit | underscore)* as cname {
      printf "cname: %s\n" cname;
      CNAME (cname);
    }
  | digit+ as integer_literal {
      printf "integer_literal: %d\n" (int_of_string integer_literal);
      INTEGER_LITERAL (int_of_string integer_literal);
    }
  | _ as c { print_char '@'; print_char c; token lexbuf}
  | eof     { printf "EOF"; EOF }

and comment_token comment = parse
  | multi_line_comment_end  {
      printf "comment: %s\n" comment;
      printf "multi line comment end\n";
      token lexbuf;
    }
  | _ as c {
      comment_token (comment ^ Char.escaped(c)) lexbuf
    }

{
  (*
  let main () =
    let lexbuf = Lexing.from_channel stdin in
    while true do
      token lexbuf
    done

  let _ = Printexc.print main ()
  *)
}