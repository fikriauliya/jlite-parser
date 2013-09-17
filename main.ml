open Parser
open Interface
open Printf

let current_indentation = ref 0

let string_of_indentation() : string =
  "\n" ^ (String.make !current_indentation ' ') ^ (string_of_int !current_indentation)

let string_of_list lst func delim =
  current_indentation := !current_indentation + 1;
  let res = string_of_indentation() ^ (String.concat delim (List.map func lst)) in
  begin
    current_indentation := !current_indentation - 1;
    res;
  end

let string_of_jlite_type (jlite_type_v: jlite_type) : string =
  current_indentation := !current_indentation + 1;
  let res = string_of_indentation() ^ (
    match jlite_type_v with
      IntT -> "Int"
      | BoolT -> "Bool"
      | StringT -> "String"
      | ObjectT class_name -> "Object of " ^ class_name
      | VoidT -> "Void"
      | Unknown -> "Unknown"
    ) in
  begin
    current_indentation := !current_indentation - 1;
    res;
  end

let string_of_jlite_op (jlite_op_v: jlite_op) : string =
  current_indentation := !current_indentation + 1;
  let res = string_of_indentation() ^ (
    match jlite_op_v with
      BooleanOp (string_v) -> (string_v)
      | RelationalOp (string_v) -> (string_v)
      | AritmeticOp (string_v) -> (string_v)
      | UnaryOp (string_v) -> (string_v)
    ) in
  begin
    current_indentation := !current_indentation - 1;
    res;
  end

let string_of_var_id (var_id_v: var_id) : string =
  current_indentation := !current_indentation + 1;
  let res = string_of_indentation() ^ (
    match var_id_v with
      (SimpleVarId s) -> s
      | _ -> "Not implemented"
    ) in
  begin
    current_indentation := !current_indentation - 1;
    res;
  end

let rec string_of_jlite_expr (jlite_expr_v: jlite_exp) : string =
  current_indentation := !current_indentation + 1;
  let res = string_of_indentation() ^ (
    match jlite_expr_v with
      UnaryExp (jlite_op_v, jlite_exp_v) -> 
        ((string_of_jlite_op jlite_op_v) ^
        (string_of_jlite_expr jlite_exp_v))
      | BinaryExp (jlite_op_v, jlite_exp_v_1, jlite_exp_v_2) -> 
        ((string_of_jlite_op jlite_op_v) ^
        (string_of_jlite_expr jlite_exp_v_1) ^
        (string_of_jlite_expr jlite_exp_v_2))
      | FieldAccess (jlite_exp_v, var_id_v) -> 
        ((string_of_jlite_expr jlite_exp_v) ^
        (string_of_var_id var_id_v))
      | ObjectCreate (class_name_v) -> 
        (class_name_v)
      | MdCall (jlite_exp_v, jlite_exp_v_list) -> 
        ((string_of_jlite_expr jlite_exp_v) ^
        (string_of_list jlite_exp_v_list string_of_jlite_expr ", "))
      | BoolLiteral (bool_v) -> 
        (string_of_bool bool_v)
      | IntLiteral (int_v) -> 
        (string_of_int int_v)
      | StringLiteral (string_v) -> 
        (string_v)
      | ThisWord -> 
        "this"
      | NullWord -> 
        "null"
      | Var (var_id_v) -> 
        (string_of_var_id var_id_v)
      | TypedExp (jlite_exp_v, jlite_type_v) -> 
        ((string_of_jlite_expr jlite_exp_v) ^
        (string_of_jlite_expr jlite_exp_v))
    ) in
  begin
    current_indentation := !current_indentation - 1;
    res;
  end

let rec string_of_jlite_stmt (jlite_stmt_v: jlite_stmt) : string =
  current_indentation := !current_indentation + 1;
  let res = string_of_indentation() ^ (
    match jlite_stmt_v with
      IfStmt (jlite_exp_v, jlite_stmt_list_v_1, jlite_stmt_list_v_2) -> 
        ((string_of_jlite_expr jlite_exp_v) ^ 
        (string_of_list jlite_stmt_list_v_1 string_of_jlite_stmt ", ") ^ 
        (string_of_list jlite_stmt_list_v_2 string_of_jlite_stmt ", "))
      | WhileStmt (jlite_exp_v, jlite_stmt_list_v) -> 
        ((string_of_jlite_expr jlite_exp_v) ^ 
        (string_of_list jlite_stmt_list_v string_of_jlite_stmt ", "))
      | ReadStmt var_id_v -> (string_of_var_id var_id_v)
      | PrintStmt jlite_exp_v -> (string_of_jlite_expr jlite_exp_v)
      | AssignStmt (var_id_v, jlite_exp_v) -> 
        ((string_of_var_id var_id_v) ^ (string_of_jlite_expr jlite_exp_v))
      | AssignFieldStmt (jlite_exp_v_1, jlite_exp_v_2) -> 
        ((string_of_jlite_expr jlite_exp_v_1) ^ (string_of_jlite_expr jlite_exp_v_2))
      | MdCallStmt jlite_exp_v -> (string_of_jlite_expr jlite_exp_v)
      | ReturnStmt jlite_exp_v -> (string_of_jlite_expr jlite_exp_v)
      | ReturnVoidStmt -> "Void"
    ) in
  begin
    current_indentation := !current_indentation - 1;
    res;
  end

let string_of_var_decl (var_decl_v: var_decl) : string =
  current_indentation := !current_indentation + 1;
  let res = string_of_indentation() ^ (
    match var_decl_v with
      (jlite_type, var_id) -> ((string_of_jlite_type jlite_type) ^ (string_of_var_id var_id))
    ) in
  begin
    current_indentation := !current_indentation - 1;
    res;
  end

let string_of_arg_decl (var_decl_v: var_decl) : string =
  current_indentation := !current_indentation + 1;
  let res = string_of_indentation() ^ (
    match var_decl_v with
      (jlite_type, var_id) -> ((string_of_jlite_type jlite_type) ^ (string_of_var_id var_id))
    ) in
  begin
    current_indentation := !current_indentation - 1;
    res;
  end

let string_of_md_decl (md_decl_v: md_decl) : string =
  current_indentation := !current_indentation + 1;
  let res = string_of_indentation() ^
    (string_of_var_id md_decl_v.jliteid) ^
    (string_of_var_id md_decl_v.ir3id) ^
    (string_of_jlite_type md_decl_v.rettype) ^
    (string_of_list md_decl_v.params string_of_arg_decl ", ") ^
    (string_of_list md_decl_v.localvars string_of_var_decl ", ") ^
    (string_of_list md_decl_v.stmts string_of_jlite_stmt ", ") in
  begin
    current_indentation := !current_indentation - 1;
    res    
  end

let string_of_class_main (class_main_v: class_main) : string =
  current_indentation := !current_indentation + 1;
  let res = string_of_indentation() ^ (match class_main_v with
    (class_name_v, md_decl_v) -> class_name_v ^ (string_of_md_decl md_decl_v))
    in
  begin
    current_indentation := !current_indentation - 1;
    res
  end

let string_of_class_decl (class_decl_v: class_decl) : string =
  string_of_indentation() ^
  match class_decl_v with
    _ -> "a"

let string_of_jlite_program (program: jlite_program) : string = 
  match program with
    (class_main, class_decl_list) -> 
      string_of_class_main(class_main) ^ (string_of_list class_decl_list string_of_class_decl ", ")

let main () =
  try
    let lexbuf = Lexing.from_channel stdin in
      let program = Parser.program Lexer.token lexbuf in
        let out_string = string_of_jlite_program program in
          begin
            printf "===================================================\n";
            printf "%s\n" out_string;
          end

  with End_of_file -> exit 0
      
let _ = Printexc.print main ()