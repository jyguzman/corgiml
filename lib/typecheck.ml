(* open Ast
open Result
open Env  *)

(* let (let+) r f = match r with 
  Ok v -> f v 
| Error e -> Error e    *)

(* type type_error =  
  | Type_mismatch of string
  | Unrecognized_operation of string


module TypeVar = struct 
  type t 
  let count = ref 0

  let fresh_var () = 
    let var = "t" ^ string_of_int !count in 
    let _ = count := !count + 1 
    in Var var
end  *)

(* module TypeChecker (F: Error.FORMATTER) = struct 
  let substitute env name = 
    match Env.get env name with 
        None -> Ast.Var name
      | Some typ -> 
        match typ with 
          Env.VarBind b -> b

  let rec check_expr env expr = 
    let expr_str = F.display_expr expr in
    match expr.Ast.expr_desc with 
    | Integer _ -> Ok int 
    | Float _ -> Ok float
    | String _ -> Ok string
    | Bool _ -> Ok bool
    | Ident i -> 
      (match Tenv.get env i with 
        None -> Error (Unrecognized_operation (Printf.sprintf "%s\n Could not find a previous decalaration for the variable '%s'" expr_str i))
      | Some typ -> Ok typ)

    | Grouping g -> check_expr env g
    
    | If (condition, then_expr, else_expr) ->  
      let* cond_type = check_expr env condition in 
      begin match cond_type with 
        | App(TBool, []) -> 
            let* then_expr_type = check_expr env then_expr in 
            (match else_expr with 
              None -> 
                if then_expr_type <> unit then 
                  Error (Type_mismatch (expr_str ^ "\n" ^ "no else branch (returns unit)"))
                else
                  Ok unit
            | Some expr ->  
              let* else_expr_type = check_expr env expr in 
              if else_expr_type = then_expr_type then 
                Ok else_expr_type 
              else 
                Error (Type_mismatch (expr_str ^ "\n" ^ "if then and else must be same type")))
        | _ -> 
          Error (Type_mismatch (expr_str ^ "\n" ^ "if condition must be boolean"))
      end

    (* | Function (idents, body, _) -> 
      let idents = in
      Ok int *)

    | Apply(fn, args) ->
      let* fn_type = check_expr env fn in 
      let _ = print_endline ("fn type: " ^ string_of_type fn_type) in
      let _ = print_endline ("fn: " ^ stringify_expr fn) in
      (match fn_type with 
        | App(TArrow, [first; _]) ->
            begin match args with 
                [] -> Error (Type_mismatch "not enough args")
              | arg :: rest_args -> 
                let* arg_type = check_expr env arg in
                if arg_type <> first then 
                  Error (Type_mismatch (expr_str ^ "types don't match"))
                else 
                  let app = Apply({expr_desc = Apply(fn, [arg]); loc = fn.loc}, rest_args) in
                  check_expr env {expr_desc = app; loc = fn.loc}
            end
        | _ -> 
          Error (Type_mismatch "not a function"))

    | Binary (l, op, r) -> 
      let* l_type = check_expr env l in 
      let* r_type = check_expr env r in 
      begin match op with 
        "+" | "-" | "*" | "/" ->
          (match l_type, r_type with 
            | App(TInt, []), App(TInt, []) -> Ok (App(TInt, []))
            | _, _ -> Error (Type_mismatch (expr_str ^ Error.bin_op_error_str op l r)))
  
        | "+." | "-." | "*." | "/." ->
          (match l_type, r_type with 
            | App(TFloat, []), App(TFloat, []) -> Ok (App(TFloat, []))
            | _, _ -> Error (Type_mismatch (Error.bin_op_error_str op l r))) 
  
        | "<" | "<=" | "=" | ">" | ">=" | "<>" ->
          (match l_type, r_type with  
            | App(TBool, []), _ | _, App(TBool, []) -> Error (Type_mismatch (Error.bin_op_error_str op l r))
            | _, _ -> Ok (App(TBool, [])))
  
        | "&&" | "||" -> 
          (match l_type, r_type with  
            | App(TBool, []), App(TBool, []) -> Ok (App(TBool, []))
            | _, _ ->  Error (Type_mismatch (Error.bin_op_error_str op l r)))
             
        | _ -> 
          Error (Unrecognized_operation (expr_str ^ "unrecognized binary operator" ^ op))
      end 

    | Ast.Let (_, value_binding, body) -> 
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
          let new_env = Tenv.add env i rhs_type in
            check_expr new_env body
        | _ -> Error (Unrecognized_operation ("Unrecognized pattern " ^ (Ast.stringify_pattern value_binding.pat)))
      end
    | _ -> Error (Unrecognized_operation ("Operation " ^ (Ast.stringify_expr expr) ^ " not supported"))    
  
  let check_module_item env mi = 
    match mi.module_item_desc with
      Ast.Expr expr -> check_expr env expr
    | Ast.LetDeclaration (_, value_binding) -> 
      let* rhs_type = check_expr env value_binding.rhs in
      (match value_binding.pat.pattern_desc with 
      | Wildcard -> Ok unit
      | EmptyParens -> 
        if rhs_type = unit then 
          Ok unit 
        else 
          Error (Type_mismatch "needs to be unit")
      | Ast.ConstIdent i -> 
        let _ = Tenv.add env i rhs_type in
          Ok unit
      | _ -> Error (Unrecognized_operation ("Unrecognized pattern " ^ (Ast.stringify_pattern value_binding.pat))))
    | _ -> Error (Unrecognized_operation "")
end 
 *)
