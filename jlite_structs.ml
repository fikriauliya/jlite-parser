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