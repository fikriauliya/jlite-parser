open Parser
open Interface

let main () =
  try
    let lexbuf = Lexing.from_channel stdin in
      let _ = Parser.program Lexer.token lexbuf in
        ()
  with End_of_file -> exit 0
      
let _ = Printexc.print main ()