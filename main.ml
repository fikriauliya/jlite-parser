open Parser
open Interface
open Printf
open Displayfunc

let main () =
  try
    let lexbuf = Lexing.from_channel stdin in
      let program = Parser.program Lexer.token lexbuf in
        let out_string = Displayfunc.string_of_jlite_program program in
          begin
            printf "===================================================\n";
            printf "%s\n" out_string;
          end

  with End_of_file -> exit 0
      
let _ = Printexc.print main ()