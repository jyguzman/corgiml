
open Type

let (let*) r f = match r with 
  Ok v -> f v 
| Error e -> Error e   

type type_error = 
  | Type_mismatch of string
  | Unrecognized_operation of string

let check_literal = function 
    Ast.Integer _ -> App(TInt, [])
  | Ast.Float _ -> App(TFloat, [])
  | Ast.String _ -> App(TString, [])
  | Ast.Bool _ -> App(TBool, [])
  | Ast.Ident _ -> App(TString, [])

let rec check_expr expr = 
  match expr.Ast.expr_desc with 
  | Ast.Literal l -> Ok (check_literal l)
  | Grouping g -> check_expr g
  | Ast.BinOp op -> 
    begin match op with 
      Ast.IAdd (l, r) | Ast.ISubtract (l, r) | Ast.IDivide (l, r) | Ast.IMultiply (l, r) ->
        let* l_type = check_expr l in 
        let* r_type = check_expr r in 
        (match l_type, r_type with 
          | App(TInt, []), App(TInt, []) -> Ok (App(TInt, []))
          | _, _ -> Error (Type_mismatch (Error.int_bin_op_error_str op l r)))

      | Ast.FAdd (l, r) | Ast.FSubtract (l, r) | Ast.FDivide (l, r) | Ast.FMultiply (l, r) ->
        let* l_type = check_expr l in 
        let* r_type = check_expr r in 
        (match l_type, r_type with 
          | App(TFloat, []), App(TFloat, []) -> Ok (App(TFloat, []))
          | _, _ -> Error (Type_mismatch (Error.float_bin_op_error_str op l r))) 

      | Ast.Eq (l, r) | Ast.Neq (l, r) | Ast.Less (l, r) | Ast.Leq (l, r) | Ast.Greater (l, r) | Ast.Geq (l, r) ->
        let* l_type = check_expr l in 
        let* r_type = check_expr r in 
        (match l_type, r_type with  
          | Nil, _ | _, Nil 
          | App(TBool, []), _ | _, App(TBool, []) -> Error (Type_mismatch (Error.compare_error_str op l r))
          | _, _ -> Ok (App(TBool, [])))

      | Ast.And (l, r) | Ast.Or (l, r) ->
        let* l_type = check_expr l in 
        let* r_type = check_expr r in 
        (match l_type, r_type with  
          | App(TBool, []), App(TBool, []) -> Ok (App(TBool, []))
          | _, _ ->  Error (Type_mismatch (Error.logical_error_str op l r)))
    end
  | _ -> Error (Unrecognized_operation ("Operation " ^ (Ast.stringify_expr expr) ^ " not supported"))

let check_module_item mi = match mi with 
    Ast.Expr e -> check_expr e
  | Ast.LetDeclaration (_, _, _) -> Ok Nil 
  | _ -> Error (Unrecognized_operation "")