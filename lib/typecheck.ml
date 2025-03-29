open Ast
open Env  

type type_error =  
  | Type_mismatch of string
  | Unrecognized_operation of string

let (let*) r f = match r with 
  Ok v -> f v 
| Error e -> Error e   

type type_constraint = 
  | BinaryOpConstraint of expression * ty * ty * ty (* left type, right type, expected type *)
  | UnaryOpConstraint of expression * ty * ty (* operand type, expected type *)
  
  | IfConditionConstraint of expression * ty (* condition of an if expression must be Bool *)
  | IfBranchesConstraint of expression * ty * ty (* these types must match *)

let type_of_pattern env pattern =  
  match pattern.pattern_desc with 
    Const_integer _ -> Ok int 
  | Const_float _ -> Ok float 
  | Const_string _ -> Ok string 
  | True | False -> Ok bool 
  | Empty_parens -> Ok unit 
  | Const_ident name ->  
    (match Type_env.lookup name env with 
      | Some ty -> Ok ty
      | None ->  
        Error (Unrecognized_operation (Printf.sprintf "Could not find a previous declaration for the variable '%s'" name)))
  | Any -> Ok Any
  | _ -> Error (Type_mismatch (Printf.sprintf "this is not a valid pattern"))  


module TypeChecker (F: Error.FORMATTER) = struct 

  let rec check_bindings env value_bindings = 
    let rec check_bindings_aux checked_bindings constraints rest = 
      if List.length rest = 0 then 
        Ok (checked_bindings, constraints)
      else
        let vb = List.hd rest in 
        let* typ, cons = check_expr env vb.rhs in 
        let constraints = constraints @ cons in
        match vb.pat.pattern_desc with 
          | Const_ident i -> 
            check_bindings_aux ((i, typ) :: checked_bindings) constraints (List.tl rest)
          | Any -> 
            check_bindings_aux checked_bindings constraints (List.tl rest)
          | Empty_parens -> 
            if typ = unit then 
              check_bindings_aux checked_bindings constraints (List.tl rest)
            else 
              Error (Type_mismatch "needs to be unit")
          | _ -> 
            Error (Type_mismatch "unrecognized pattern")
    in 
    let* bindings, constraints = check_bindings_aux [] [] value_bindings in 
    Ok (Type_env.add env bindings, constraints)

  and check_expr env expr = 
    let expr_str = F.display_expr expr in
    match expr.Ast.expr_desc with 
    | Integer _ -> Ok (int, [])
    | Float _ -> Ok (float, [])
    | String _ -> Ok (string, [])
    | Bool _ -> Ok (bool, [])
    | Ident name -> 
      (match Type_env.lookup name env with 
        | Some ty -> Ok (ty, [])
        | None -> 
          Error (Unrecognized_operation (Printf.sprintf "%s\n Could not find a previous declaration for the variable '%s'" expr_str name)))

    | Grouping g -> check_expr env g 

    (* | Function (params, body, _) -> 
      let vars = List.map (fun p -> (p, Var.fresh ())) params in 
      let env = Type_env.add env vars in 
      let* ret = check_expr env body in 
        Arrow() *)
        
    | If (cond, then_exp, else_expr) -> 
      let* cond_typ, cond_cons = check_expr env cond in 
      let* then_typ, then_cons = check_expr env then_exp in
      let* else_typ, else_cons = check_expr env else_expr in 
      let constraints = cond_cons @ then_cons @ else_cons in 
      let cond_con = IfConditionConstraint (expr, cond_typ) in 
      let if_con = IfBranchesConstraint (expr, then_typ, else_typ) in 
        Ok (then_typ, constraints @ (if_con :: [cond_con]))  
          (* assume the true result has the correct type *)

    (* | Match (exp, cases) -> 
      let* exp_typ, exp_cons = check_expr env expr in  *)

    | Binary (l, (op, _op_loc), r) -> 
      let* l_typ, l_cons = check_expr env l in 
      let* r_typ, r_cons = check_expr env r in 
      let* expected_typ = begin match op with 
        "+" | "-" | "*" | "/" -> 
          Ok int
        | "+." | "-." | "*." | "/." -> 
          Ok float
        | "<" | "<=" | "=" | ">" | ">=" | "<>" 
          -> Ok l_typ (* Left & right types must be equal, so just assume the left has correct type *)
        | "&&" | "||" -> 
          Ok bool
        | "^" ->  
          Ok string
        | _ -> 
          Error (Unrecognized_operation (expr_str ^ "unrecognized binary operator" ^ op))
      end in
      Ok (expected_typ, BinaryOpConstraint (expr, l_typ, r_typ, expected_typ) :: (l_cons @ r_cons))

    | Ast.Let (_, value_bindings, body) -> 
      let* let_env, constraints = check_bindings env value_bindings in
      let* body_typ, body_cons = check_expr let_env body in 
      Ok (body_typ, constraints @ body_cons) 
    | _ -> 
      Error (Unrecognized_operation ("Operation " ^ (Ast.stringify_expr expr) ^ " not supported"))    

  (* let rec generate_constraints env expr = match expr.expr_desc with 
    | Integer _ -> Ok (int, []) 
    | Float _ -> Ok (float, [])
    | String _ -> Ok (string, [])
    | Bool _ -> Ok (bool, [])
    | Ident _ -> 
      let* typ = check_expr env expr in Ok (typ, [])
    | If (c, l, _) -> 
      let* c, c_cons = generate_constraints env c in 
      let cond_constr = [Eq (c, bool)] in 
      let* l, l_cons = generate_constraints env l in 
      let branch_constraints = l_cons in
      let constraints = List.append cond_constr branch_constraints in
      (* let* r, r_cons = generate_constraints env r in  *)
        Ok (l, constraints)
    | _ -> 
      Error (Unrecognized_operation "gen_con: not an expr") *)
  
  (* let check_module_item env mi = 
    match mi.module_item_desc with
      Ast.Expr expr -> 
        let* _, _ = check_expr env expr in Ok env
    | Ast.LetDeclaration (_, value_bindings) -> 
        check_bindings env value_bindings 
    | _ -> 
      Error (Unrecognized_operation "not valid module binding")

  let check_program init_env module_items = 
    let rec check_program_aux env module_items = 
      if List.length module_items = 0 then 
        Ok ()
      else
        let current_mi = List.hd module_items in 
        let* new_env = check_module_item env current_mi in 
        check_program_aux new_env (List.tl module_items)
    in 
      check_program_aux init_env module_items *)

  (* and get_case_constraints cases = 
    let pat, expr = ca.lhs, match_case.rhs in  *)

      
end 

