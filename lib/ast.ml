type pattern = 
  | ConstInteger of int 
  | ConstFloat of float 
  | ConstString of string 
  | True
  | False
  | ConstIdent of string
  | EmptyBrackets 
  | EmptyParens
  | Wildcard
  | Cons of string * string

type expr = 
  | None
  | Integer of int 
  | Float of float 
  | String of string 
  | Bool of bool
  | Ident of string 
  | BinOp of bin_op 
  | UnOp of un_op 
  | Grouping of expr

  | IfExpr of {
    then_cond: expr; 
    then_expr: expr;
    else_expr: expr;
  }
  | LetBinding of bool * string * expr * expr option
  | Function of {
    param: string; 
    expr: expr;
  }
  | FnApp of {
    fn_name: string;
    arg: expr;
  }
  | MatchClause of match_clause

  | PatternMatch of {
    match_expr: expr;
    clauses: match_clause list;
  }

  and match_clause = {
    pattern: pattern;
    cmp_to: expr;
  }

and bin_op = 
  | IAdd of expr * expr 
  | IMultiply of expr * expr 
  | ISubtract of expr * expr 
  | IDivide of expr * expr
  | FAdd of expr * expr 
  | FMultiply of expr * expr 
  | FSubtract of expr * expr 
  | FDivide of expr * expr
  | Less of expr * expr
  | Leq of expr * expr
  | Greater of expr * expr
  | Geq of expr * expr

and un_op = 
  | IPower of expr
  | FPower of expr

type module_item =
  | Expr of expr
  | TypeDefintion of type_definition 

and type_definition = {
  type_name: string;
  type_constructors: typ_con list
}

and typ_con = {
  var_name: string;
  of_type: typ option
}

and typ = 
  | TUnit
  | TInt 
  | TFloat 
  | TString 
  | TBool 
  | TNone
  | TFunction of fun_typ 
  
and fun_typ = {
  param_typs: typ list;
  ret_type: typ
} 

let rec stringify_type typ = match typ with 
    TUnit -> "unit" | TInt -> "int" | TFloat -> "float" 
  | TString -> "string" | TBool -> "bool" | TNone -> "None"
  | TFunction tf -> 
    let param_str = List.fold_left (fun acc x -> acc ^ (stringify_type x) ^ "->") "(" tf.param_typs in 
      Printf.sprintf "%s) -> %s" param_str (stringify_type tf.ret_type)

let rec stringify_module_item mi = match mi with 
    Expr e -> stringify_expr e 
  | TypeDefintion td ->
    let type_cons = List.fold_left (fun acc x -> acc ^ "|" ^ (stringify_type_con x)) "" td.type_constructors in
      Printf.sprintf "TypeDefinition(%s = %s)" td.type_name type_cons

and stringify_type_con v = 
  let typ_string = match v.of_type with None -> "" | Some t -> " of " ^ stringify_type t in 
    Printf.sprintf "%s%s" v.var_name typ_string  

and stringify_expr expr = match expr with 
    String s -> Printf.sprintf "\"%s\"" s
  | Integer i -> string_of_int i 
  | Float f -> string_of_float f
  | Bool b -> string_of_bool b
  | Ident i -> Printf.sprintf "Id(%s)" i 
  | BinOp b -> (match b with 
      IAdd (l, r) -> Printf.sprintf "IAdd(%s, %s)" (stringify_expr l) (stringify_expr r) 
    | IMultiply (l, r) -> Printf.sprintf "IMultiply(%s, %s)" (stringify_expr l) (stringify_expr r) 
    | IDivide (l, r) -> Printf.sprintf "IDivide(%s, %s)" (stringify_expr l) (stringify_expr r) 
    | ISubtract (l, r) -> Printf.sprintf "ISubtract(%s, %s)" (stringify_expr l) (stringify_expr r)
    | FAdd (l, r) -> Printf.sprintf "FAdd(%s, %s)" (stringify_expr l) (stringify_expr r) 
    | FMultiply (l, r) -> Printf.sprintf "FMultiply(%s, %s)" (stringify_expr l) (stringify_expr r) 
    | FDivide (l, r) -> Printf.sprintf "FDivide(%s, %s)" (stringify_expr l) (stringify_expr r) 
    | FSubtract (l, r) -> Printf.sprintf "FSubtract(%s, %s)" (stringify_expr l) (stringify_expr r)
    | Less (l, r) -> Printf.sprintf "Less(%s, %s)" (stringify_expr l) (stringify_expr r)
    | Leq (l, r) -> Printf.sprintf "Lew(%s, %s)" (stringify_expr l) (stringify_expr r)
    | Greater (l, r) -> Printf.sprintf "Greater(%s, %s)" (stringify_expr l) (stringify_expr r)
    | Geq (l, r) -> Printf.sprintf "Geq(%s, %s)" (stringify_expr l) (stringify_expr r))
  | Grouping g -> stringify_expr g
  | LetBinding (is_rec, pat, l, r) -> 
    let rec_str = if is_rec then "rec " else "" in 
    let in_string = (match r with None -> "" | Some e -> " in " ^ stringify_expr e) in  
      Printf.sprintf "Let(%s%s = %s%s)" rec_str pat (stringify_expr l) in_string
  | IfExpr i -> 
      Printf.sprintf "If(%s then %s else %s)" (stringify_expr i.then_cond) (stringify_expr i.then_expr) (stringify_expr i.else_expr)
  | Function f -> 
      Printf.sprintf "Fun(%s -> %s)" f.param (stringify_expr f.expr)
  | PatternMatch pm -> 
      Printf.sprintf "PatternMatch(match %s with %s)" (stringify_expr pm.match_expr) (stringify_match_clauses pm.clauses)
  | FnApp fa -> 
      Printf.sprintf "FnApp(fn_name = %s, arg = %s)" fa.fn_name (stringify_expr fa.arg)
  | None | _ -> ""

and stringify_params params = List.fold_left (fun acc x -> acc ^ x ^ " ") "" params

and stringify_pattern pat = match pat with 
    ConstInteger i -> string_of_int i 
  | ConstFloat f -> string_of_float f 
  | ConstString s -> s 
  | ConstIdent i -> Printf.sprintf "Id(%s)" i 
  | True -> "true"
  | False -> "false" 
  | EmptyBrackets -> "[]"
  | EmptyParens -> "()"
  | Wildcard -> "_"
  | Cons (l, r) -> Printf.sprintf "(%s :: %s)" l r


and stringify_match_clause clause = 
  Printf.sprintf "| %s -> %s" (stringify_pattern clause.pattern) (stringify_expr clause.cmp_to)

and stringify_match_clauses clauses = 
  List.fold_left (fun acc clause -> acc ^ (stringify_match_clause clause)) "" clauses

let stringify_program module_items = 
  List.fold_left (fun acc x -> acc ^ (stringify_module_item x) ^ "\n") "" module_items