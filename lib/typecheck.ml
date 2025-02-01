open Type

let (let*) r f = match r with 
  Ok v -> f v 
| Error e -> Error e   

type type_error = 
  | Type_mismatch of string
  | Unrecognized_operation of string

module TypeChecker (F: Error.FORMATTER) = struct 
  let rec check_expr env expr = 
    let expr_str = F.display_expr expr in
    match expr.Ast.expr_desc with 
    | Ast.Literal Integer _ -> Ok int 
    | Ast.Literal Float _ -> Ok float
    | Ast.Literal String _ -> Ok string
    | Ast.Literal Bool _ -> Ok bool
    | Ast.Literal Ident i -> 
      (match Env.get env i with 
        None -> Error (Unrecognized_operation (Printf.sprintf "%s Could not find a previous decalaration for the variable '%s'" expr_str i))
      | Some typ -> 
        match typ with 
          Env.VarBind b -> Ok b)
    | Grouping g -> check_expr env g
    | Ast.IfExpr (condition, then_expr, else_expr) -> 
      let* cond_type = check_expr env condition in 
      begin match cond_type with 
        | App(TBool, []) -> 
            let* then_expr_type = check_expr env then_expr in 
            (match else_expr with 
              None -> Ok then_expr_type
            | Some expr ->  
              let* else_expr_type = check_expr env expr in 
              if else_expr_type = then_expr_type then 
                Ok else_expr_type 
              else 
                Error (Type_mismatch "if then and else must be same type"))
        | _ -> 
          Error (Type_mismatch (expr_str ^ "\n" ^ "if condition must be boolean"))
      end
      | Ast.BinOp (l, op, r) -> 
        let* l_type = check_expr env l in 
        let* r_type = check_expr env r in 
        begin match op with 
          "+" | "-" | "*" | "/" ->
            (match l_type, r_type with 
              | App(TInt, []), App(TInt, []) -> Ok (App(TInt, []))
              | _, _ -> Error (Type_mismatch (expr_str ^ Error.int_bin_op_error_str op l r)))
    
          | "+." | "-." | "*." | "/." ->
            (match l_type, r_type with 
              | App(TFloat, []), App(TFloat, []) -> Ok (App(TFloat, []))
              | _, _ -> Error (Type_mismatch (Error.float_bin_op_error_str op l r))) 
    
          | "<" | "<=" | "=" | ">" | ">=" | "<>" ->
            (match l_type, r_type with  
              | App(TBool, []), _ | _, App(TBool, []) -> Error (Type_mismatch (Error.compare_error_str op l r))
              | _, _ -> Ok (App(TBool, [])))
    
          | "&&" | "||" ->
            (match l_type, r_type with  
              | App(TBool, []), App(TBool, []) -> Ok (App(TBool, []))
              | _, _ ->  Error (Type_mismatch (Error.logical_error_str op l r)))
              
          | _ -> 
            Error (Unrecognized_operation (expr_str ^ "unrecognized binary operator" ^ op))
        end
      | Ast.LetBinding (_, value_binding, body) -> 
        let* rhs_type = check_expr env value_binding.rhs in
        let* body = match body with None -> Error (Unrecognized_operation "body empty") | Some b -> Ok b in
        begin match value_binding.pat.pattern_desc with 
            Wildcard -> check_expr env body
          | EmptyParens -> 
            if rhs_type = unit then 
              check_expr env body  
            else 
              Error (Type_mismatch "needs to be unit")
          | Ast.ConstIdent i -> 
            let binding = Env.VarBind rhs_type in
            let new_env = Env.add env i binding in
              check_expr new_env body
          | _ -> Error (Unrecognized_operation ("Unrecognized pattern " ^ (Ast.stringify_pattern value_binding.pat)))
        end
    | _ -> Error (Unrecognized_operation ("Operation " ^ (Ast.stringify_expr expr) ^ " not supported"))
  
  let check_module_item mi env = match mi with 
      Ast.Expr expr -> check_expr env expr
    | Ast.LetDeclaration (_, value_binding, _) -> 
      let* rhs_type = check_expr env value_binding.rhs in
      (match value_binding.pat.pattern_desc with 
      | Wildcard -> Ok unit
      | EmptyParens -> 
        if rhs_type = unit then 
          Ok unit 
        else 
          Error (Type_mismatch "needs to be unit")
      | Ast.ConstIdent i -> 
        let binding = Env.VarBind rhs_type in
        let _ = Env.add env i binding in
          Ok unit
      | _ -> Error (Unrecognized_operation ("Unrecognized pattern " ^ (Ast.stringify_pattern value_binding.pat))))
    | _ -> Error (Unrecognized_operation "")
end 

