
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
        None -> Error (Unrecognized_operation ("coult not find" ^ i))
      | Some typ -> 
        match typ with 
          Env.VarBind b -> Ok b)
    | Grouping g -> check_expr env g
    | Ast.BinOp op -> 
      begin match op with 
        Ast.IAdd (l, r) | Ast.ISubtract (l, r) | Ast.IDivide (l, r) | Ast.IMultiply (l, r) ->
          let* l_type = check_expr env l in 
          let* r_type = check_expr env r in 
          (match l_type, r_type with 
            | App(TInt, []), App(TInt, []) -> Ok (App(TInt, []))
            | _, _ -> Error (Type_mismatch (expr_str ^ Error.int_bin_op_error_str op l r)))
  
        | Ast.FAdd (l, r) | Ast.FSubtract (l, r) | Ast.FDivide (l, r) | Ast.FMultiply (l, r) ->
          let* l_type = check_expr env l in 
          let* r_type = check_expr env r in 
          (match l_type, r_type with 
            | App(TFloat, []), App(TFloat, []) -> Ok (App(TFloat, []))
            | _, _ -> Error (Type_mismatch (Error.float_bin_op_error_str op l r))) 
  
        | Ast.Eq (l, r) | Ast.Neq (l, r) | Ast.Less (l, r) | Ast.Leq (l, r) | Ast.Greater (l, r) | Ast.Geq (l, r) ->
          let* l_type = check_expr env l in 
          let* r_type = check_expr env r in 
          (match l_type, r_type with  
            | App(TBool, []), _ | _, App(TBool, []) -> Error (Type_mismatch (Error.compare_error_str op l r))
            | _, _ -> Ok (App(TBool, [])))
  
        | Ast.And (l, r) | Ast.Or (l, r) ->
          let* l_type = check_expr env l in 
          let* r_type = check_expr env r in 
          (match l_type, r_type with  
            | App(TBool, []), App(TBool, []) -> Ok (App(TBool, []))
            | _, _ ->  Error (Type_mismatch (Error.logical_error_str op l r)))
      end
      | Ast.LetBinding (_, pattern, rhs, body) -> 
        let* ident = match pattern.pattern_desc with 
          Ast.ConstIdent i -> Ok i 
        | _ -> Error (Unrecognized_operation "Not anl ident")
        in
        let* rhs_type = check_expr env rhs in 
        let binding = Env.VarBind rhs_type in
        let new_env = Env.add env ident binding in
        let* body = match body with None -> Error (Unrecognized_operation "body empty") | Some b -> Ok b in
          check_expr new_env body
    | _ -> Error (Unrecognized_operation ("Operation " ^ (Ast.stringify_expr expr) ^ " not supported"))
  
  let check_module_item mi env = match mi with 
      Ast.Expr expr -> check_expr env expr
    | Ast.LetDeclaration (_, pattern, rhs) -> 
      let* ident = match pattern.pattern_desc with 
        Ast.ConstIdent i -> Ok i 
      | _ -> Error (Unrecognized_operation "Not an ident")
      in
      let* rhs_type = check_expr env rhs in 
      let binding = Env.VarBind rhs_type in
      let _ = Env.add env ident binding in
        Ok unit 
    | _ -> Error (Unrecognized_operation "")
end 

