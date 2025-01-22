let (let+) r f = match r with 
  Ok v -> f v 
| Error e -> Error e   

type type_error = 
  | Type_mismatch of string
  | Unrecognized_operation of string

let check_int e = match e.Ast.expr_desc with  
  Ast.Literal Integer _ -> true
| _ -> false

let check_float e = match e.Ast.expr_desc with  
    Ast.Literal Float _ -> true
  | _ -> false

let get_literal_type = function 
    Ast.Literal Integer _ -> "int"
  | Ast.Literal Float _ -> "float"
  | Ast.Literal String _ -> "string"
  | Ast.Literal Bool _ -> "bool" 
  | _ -> ""


(* let check_add l r = 
  let l_t, r_t = get_literal_type l, get_literal_type r in
  (match check_int l, check_int r with 
    | None, Some r -> Error (Type_mismatch (Printf.sprintf "Line %d: '%s' expects two expressions of type int, but %s is of type %s" (Ast.op op) (Ast.stringify_expr l) (get_literal_type l)))
    | Some _, None -> Error (Type_mismatch ("Expected int but got " ^ Ast.stringify_expr r))
    | None, None -> Ok ()
    | Some _, Some _ -> Ok ()) *)

(* let check_expr expr = 
  match expr with 
    Ast.BinOp op -> (match op with 
      Ast.IAdd (l, r) | Ast.ISubtract (l, r) | Ast.IDivide (l, r) | Ast.IMultiply (l, r) ->
        (match check_int l, check_int r with 
          | None, Some r -> Error (Type_mismatch (Printf.sprintf "Line %d: '%s' expects two expressions of type int, but %s is of type %s" (Ast.op op) (Ast.stringify_expr l) (get_literal_type l)))
          | Some _, None -> Error (Type_mismatch ("Expected int but got " ^ Ast.stringify_expr r))
          | None, None -> Ok ()
          | Some _, Some _ -> Ok ())
      | Ast.FAdd (l, r) | Ast.FSubtract (l, r) | Ast.FDivide (l, r) | Ast.FMultiply (l, r) ->
        (match check_float l, check_float r with 
          | None, Some _ -> Error (Type_mismatch ("Expected int but got " ^ Ast.stringify_expr l))
          | Some _, None -> Error (Type_mismatch ("Expected int but got " ^ Ast.stringify_expr r))
          | None, None -> Ok ()
          | Some _, Some _ -> Ok ())  
    | _ -> Error (Unrecognized_operation ("Unrecognized binary operation " ^ (Ast.stringify_expr expr))))
  | _ -> Error (Unrecognized_operation ("Operation " ^ (Ast.stringify_expr expr) ^ " not supported")) *)

(* let check_module_item mi = match mi with 
    Ast.Expr e -> check_expr e
  | Ast.LetDecl (_, _, rhs) ->
     check_expr rhs 
  | _ -> Ok () *)