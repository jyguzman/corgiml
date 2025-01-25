
let (let*) r f = match r with 
  Ok v -> f v 
| Error e -> Error e   

type type_error = 
  | Type_mismatch of string
  | Unrecognized_operation of string

let get_type_str = function 
    Ast.Literal Integer _ -> "int"
  | Ast.Literal Float _ -> "float"
  | Ast.Literal String _ -> "string"
  | Ast.Literal Bool _ -> "bool" 
  | _ -> ""

let bin_op_mismatch_template = format_of_string {|
  "%s" expects two %ss, but it got the following: 
      left: %s (type: %s)
      right: %s (type %s)
|}

let int_bin_op_error_str bin_op l r =  
  let op = Ast.op_for bin_op in 
  let word = match op with
    "+" -> "add" | "*" -> "multiply" | "-" -> "subtract" | "/" -> "divide" | _ -> ""
  in
  let float_hint = Printf.sprintf "\n\n To %s floats, use \"%s.\"" word op in
  let l_str, r_str = Ast.stringify_expr l, Ast.stringify_expr r in 
  let template = Printf.sprintf bin_op_mismatch_template op "integer" l_str (get_type_str l.expr_desc) r_str (get_type_str r.expr_desc) in
  let hint = match l.expr_desc, r.expr_desc with 
    | Ast.Literal Float _, Ast.Literal Float _ -> float_hint
    | Ast.Literal String _, Ast.Literal String _ -> 
      if op = "+" then "\n\nHint: To concatenate strings, use '^'"  else ""
    | Ast.Literal Float _, Ast.Literal Integer _ -> 
      let expr_str = if String.contains r_str ' '  then Printf.sprintf "(%s)" r_str else r_str in
      Printf.sprintf "%s\nHint: You can convert an int to a float with float_of_int %s" float_hint expr_str
    | Ast.Literal Integer _, Ast.Literal Float _ -> 
      let expr_str = if String.contains l_str ' '  then Printf.sprintf "(%s)" l_str else l_str in
      Printf.sprintf "%s\nHint: You can convert an int to a float with float_of_int %s" float_hint expr_str
    | _, _ -> ""
  in 
    template ^ hint

let float_bin_op_error_str bin_op l r =  
  let op = Ast.op_for bin_op in
  let l_str, r_str = Ast.stringify_expr l, Ast.stringify_expr r in 
  let template = Printf.sprintf bin_op_mismatch_template op "float" l_str (get_type_str l.expr_desc) r_str (get_type_str r.expr_desc) in
  let hint = match l.expr_desc, r.expr_desc with 
    | Ast.Literal String _, Ast.Literal String _ -> 
      if op = "+." then "\n\nHint: To concatenate strings, use '^'"  else ""
    | Ast.Literal Float _, Ast.Literal Integer _ -> 
      let expr_str = if String.contains r_str ' '  then Printf.sprintf "(%s)" r_str else r_str in
      Printf.sprintf "\nHint: You can convert an int to a float with float_of_int (%s)" expr_str
    | Ast.Literal Integer _, Ast.Literal Float _ -> 
      let expr_str = if String.contains l_str ' '  then Printf.sprintf "(%s)" l_str else l_str in
      Printf.sprintf "\nHint: You can convert an int to a float with float_of_int (%s)" expr_str
    | _, _ -> ""
  in 
    template ^ hint

let compare_error_str bin_op l r =  
  let op = Ast.op_for bin_op in
  let l_str, r_str = Ast.stringify_expr l, Ast.stringify_expr r in 
  let template = Printf.sprintf bin_op_mismatch_template op "boolean" l_str (get_type_str l.expr_desc) r_str (get_type_str r.expr_desc) in
  let hint = match l.expr_desc, r.expr_desc with 
    | Ast.Literal Float _, Ast.Literal Integer _ -> 
      let expr_str = if String.contains r_str ' '  then Printf.sprintf "(%s)" r_str else r_str in
      Printf.sprintf "\nHint: You can convert an int to a float with float_of_int (%s)" expr_str
    | Ast.Literal Integer _, Ast.Literal Float _ -> 
      let expr_str = if String.contains l_str ' '  then Printf.sprintf "(%s)" l_str else l_str in
      Printf.sprintf "\nHint: You can convert an int to a float with float_of_int (%s)" expr_str
    | _, _ -> ""
  in 
    template ^ hint

let logical_error_str bin_op l r =  
  let op = Ast.op_for bin_op in
  let l_str, r_str = Ast.stringify_expr l, Ast.stringify_expr r in 
  Printf.sprintf bin_op_mismatch_template op "boolean" l_str (get_type_str l.expr_desc) r_str (get_type_str r.expr_desc)

let check_literal = function 
    Ast.Integer _ -> Type.App(Type.TInt, [])
  | Ast.Float _ -> Type.App(Type.TFloat, [])
  | Ast.String _ -> Type.App(Type.TString, [])
  | Ast.Bool _ -> Type.App(Type.TBool, [])
  | Ast.Ident _ -> Type.App(Type.TString, [])

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
            | Type.App(Type.TInt, []), Type.App(Type.TInt, []) -> Ok (Type.App(Type.TInt, []))
            | _, _ -> Error (Type_mismatch (int_bin_op_error_str op l r)))

        | Ast.FAdd (l, r) | Ast.FSubtract (l, r) | Ast.FDivide (l, r) | Ast.FMultiply (l, r) ->
          let* l_type = check_expr l in 
          let* r_type = check_expr r in 
          (match l_type, r_type with 
            | Type.App(Type.TFloat, []), Type.App(Type.TFloat, []) -> Ok (Type.App(Type.TFloat, []))
            | _, _ -> Error (Type_mismatch (float_bin_op_error_str op l r)))

        | Ast.Less (l, r) | Ast.Leq (l, r) | Ast.Greater (l, r) | Ast.Geq (l, r) ->
          let* l_type = check_expr l in 
          let* r_type = check_expr r in 
          (match l_type, r_type with  
            | Type.Nil, _ | _, Type.Nil 
            | Type.App(Type.TBool, []), _ | _, Type.App(Type.TBool, []) -> Error (Type_mismatch (compare_error_str op l r))
            | _, _ -> Ok (Type.App(Type.TBool, [])))

        | Ast.And (l, r) | Ast.Or (l, r) ->
          let* l_type = check_expr l in 
          let* r_type = check_expr r in 
          (match l_type, r_type with  
            | Type.App(Type.TBool, []), Type.App(Type.TBool, []) -> Ok (Type.App(Type.TBool, []))
            | _, _ ->  Error (Type_mismatch (logical_error_str op l r)))
      end
  | _ -> Error (Unrecognized_operation ("Operation " ^ (Ast.stringify_expr expr) ^ " not supported"))

let check_module_item mi = match mi with 
    Ast.Expr e -> check_expr e
  | Ast.LetDeclaration (_, _, rhs) ->
     check_expr rhs 
  | _ -> Error (Unrecognized_operation "")