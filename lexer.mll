{
  open Printf
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
  | single_line_comment_start [^ '\n']* '\n' {
      printf "single line comment\n"
    }
  | multi_line_comment_start {
      printf "multi line comment start\n";
      comment_token "" lexbuf
    }
  | "\"" (escaped_special_charater | ("\\0" digit digit) | ("\\x" hex hex) | normal_character)* "\"" as string_literal {
      printf "string literal: %s\n" string_literal
    }
  | "true" | "false" as boolean_literal {
      printf "boolean literal: %b\n" (bool_of_string boolean_literal)
    }
  | '&' | '|' {
      printf "boolean operator\n"
    }
  | ">=" | "<=" | '>' | '<' | "!=" | "==" as relational_operator {
      printf "relational operator: %s\n" relational_operator
    }
  | '+' { 
      printf "plus\n"
    }
  | '-' { 
      printf "minus\n"
    }
  | '*' {
      printf "multiply\n"
    }
  | '/' {
      printf "divide\n"
    }
  | '^' {
      printf "exp\n"
    }
  | '!' {
      printf "negation operator\n"
    }
  | '-' {
      printf "negative operator\n"
    }
  | ['a'-'z'] (letter | digit | underscore)* as id {
      printf "id: %s\n" id;
    }
  | ['A'-'Z'] (letter | digit | underscore)* as cname {
      printf "cname: %s\n" cname;
    }
  | digit+ as integer_literal {
      printf "integer_literal: %d\n" (int_of_string integer_literal)
    }
  | [' ' '\t']  { 
      printf "white space\n"
    }
  | '\n'  { 
      printf "new line\n"
    }
  | eof     { exit 0 }

and comment_token comment = parse
  | multi_line_comment_end  {
      printf "comment: %s\n" comment;
      printf "multi line comment end\n"
    }
  | _ as c {
      comment_token (comment ^ Char.escaped(c)) lexbuf
    }

{
  let main () =
    let lexbuf = Lexing.from_channel stdin in
      while true do
        token lexbuf
      done

  let _ = Printexc.print main ()
}