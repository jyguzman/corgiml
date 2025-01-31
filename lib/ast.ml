type location = {
  line: int;
  col: int;
  start_pos: int;
  end_pos: int
}

type corgi_string = {
  bytes: bytes;
  len: int
}

type pattern = {
  pattern_desc: pattern_desc;
  loc: location
}

and pattern_desc = 
  | ConstInteger of int 
  | ConstFloat of float 
  | ConstString of string 
  | ConstIdent of string
  | True
  | False
  | EmptyBrackets 
  | EmptyParens
  | Wildcard

type expression = {
  expr_desc: expression_desc;
  loc: location
}

and expression_desc = 
  | None
  | Literal of literal
  | CorgiString of corgi_string
  | BinOp of bin_op 
  | UnOp of un_op 
  | Grouping of expression
  | IfExpr of expression * expression * expression option
  | LetBinding of bool * value_binding * expression option
  | Function of pattern list * expression
  | FnApp of expression * expression list
  | Match of expression * case list

and literal = 
  | Integer of int
  | Float of float
  | String of string
  | Bool of bool
  | Ident of string 

  and case = {
    lhs: pattern;
    rhs: expression;
  }

and bin_op = 
  | IAdd of expression * expression 
  | IMultiply of expression * expression 
  | ISubtract of expression * expression 
  | IDivide of expression * expression
  | FAdd of expression * expression 
  | FMultiply of expression * expression 
  | FSubtract of expression * expression 
  | FDivide of expression * expression
  | Eq of expression * expression
  | Neq of expression * expression
  | Less of expression * expression
  | Leq of expression * expression
  | Greater of expression * expression
  | Geq of expression * expression
  | And of expression * expression
  | Or of expression * expression

and un_op = 
  | IPower of expression
  | FPower of expression

and value_binding = {
  pat: pattern;
  rhs: expression;
  constraints: ty option list; (* eventually need to parse type constraints *)
  location: location;
}

and module_item =
  | Expr of expression
  | LetDeclaration of bool * value_binding * location
  | TypeDefintion of type_definition * location

and type_definition = {
  type_name: string;
  type_constructors: typ_con list
}

and typ_con = {
  type_def_name: string;
  con_name: string;
  of_type: ty option
}

and ty = 
    | App of tycon * ty list 
    | Var of string 
    | Poly of string list * ty

and tycon = 
    | TInt 
    | TFloat
    | TString 
    | TBool
    | TUnit 
    | TArrow 
    | TyFun of string list * ty

and ty_constraint = 
    | TyEq of string * ty
    | VarEq of string * string
  
and fun_typ = {
  param_typs: ty list;
  ret_type: ty
} 

let string_of_tycon = function
  | TInt -> "int"
  | TFloat -> "float"
  | TString -> "string"
  | TBool -> "bool"
  | TUnit -> "unit"
  | TArrow -> "arrow"
  | TyFun (_, _) -> "fun"

let string_of_type = function 
  | App(tycon, _) -> Printf.sprintf "%s" (string_of_tycon tycon)
  | _ -> ""   

let op_for = function
    IAdd (_, _) -> "+"
  | IMultiply (_, _) -> "*" 
  | ISubtract (_, _) -> "-"
  | IDivide (_, _) -> "/"
  | FAdd (_, _) -> "+."
  | FMultiply (_, _) -> "*." 
  | FSubtract (_, _) -> "-."
  | FDivide (_, _) -> "/."
  | Eq (_, _) -> "="
  | Neq (_, _) -> "<>"
  | Less (_, _) -> "<"
  | Leq (_, _) -> "<="
  | Greater (_, _) -> ">"
  | Geq (_, _) -> ">="
  | And (_, _) -> "&&"
  | Or (_, _) -> "||"
  
(* let rec stringify_type typ = match typ with 
    TUnit -> "unit" | TInt -> "int" | TFloat -> "float" 
  | TString -> "string" | TBool -> "bool" | TNone -> "None"
  | TFunction tf -> 
    let param_str = List.fold_left (fun acc x -> acc ^ (stringify_type x) ^ "->") "(" tf.param_typs in 
      Printf.sprintf "%s) -> %s" param_str (stringify_type tf.ret_type) *)

let rec stringify_module_item mi = match mi with 
    Expr e -> stringify_expr e 
  | LetDeclaration (is_rec, value_binding, _) -> 
      Printf.sprintf "LetDecl(%s %s)" (if is_rec then "rec " else "") (stringify_value_binding value_binding)
  | TypeDefintion (_, _) -> "need to stringify type"
    (* let type_cons = List.fold_left (fun acc x -> acc ^ "|" ^ (stringify_type_con x)) "" td.type_constructors in
      Printf.sprintf "TypeDefinition(%s = %s)" td.type_name type_cons *)

(* and stringify_type_con v = 
  let typ_string = match v.of_type with None -> "" | Some t -> " of " ^ stringify_type t in 
    Printf.sprintf "%s%s" v.con_name typ_string   *)

and stringify_expr_list exprs = 
  List.fold_left (fun acc expr -> acc ^ (stringify_expr expr) ^ ",") "" exprs 

and stringify_expr expr = match expr.expr_desc with 
    Literal String s -> Printf.sprintf "\"%s\"" s
  | Literal Integer i -> string_of_int i 
  | Literal Float f -> string_of_float f
  | Literal Bool b -> string_of_bool b
  | Literal Ident i -> Printf.sprintf "Id(%s)" i 
  | BinOp b -> (match b with 
      IAdd (l, r) -> Printf.sprintf "IAdd(%s, %s)" (stringify_expr l) (stringify_expr r) 
    | IMultiply (l, r) -> Printf.sprintf "IMultiply(%s, %s)" (stringify_expr l) (stringify_expr r) 
    | IDivide (l, r) -> Printf.sprintf "IDivide(%s, %s)" (stringify_expr l) (stringify_expr r) 
    | ISubtract (l, r) -> Printf.sprintf "ISubtract(%s, %s)" (stringify_expr l) (stringify_expr r)
    | FAdd (l, r) -> Printf.sprintf "FAdd(%s, %s)" (stringify_expr l) (stringify_expr r) 
    | FMultiply (l, r) -> Printf.sprintf "FMultiply(%s, %s)" (stringify_expr l) (stringify_expr r) 
    | FDivide (l, r) -> Printf.sprintf "FDivide(%s, %s)" (stringify_expr l) (stringify_expr r) 
    | FSubtract (l, r) -> Printf.sprintf "FSubtract(%s, %s)" (stringify_expr l) (stringify_expr r)
    | Eq (l, r) -> Printf.sprintf "Eq(%s, %s)" (stringify_expr l) (stringify_expr r)
    | Neq (l, r) -> Printf.sprintf "Neq(%s, %s)" (stringify_expr l) (stringify_expr r)
    | Less (l, r) -> Printf.sprintf "Less(%s, %s)" (stringify_expr l) (stringify_expr r)
    | Leq (l, r) -> Printf.sprintf "Leq(%s, %s)" (stringify_expr l) (stringify_expr r)
    | Greater (l, r) -> Printf.sprintf "Greater(%s, %s)" (stringify_expr l) (stringify_expr r)
    | Geq (l, r) -> Printf.sprintf "Geq(%s, %s)" (stringify_expr l) (stringify_expr r)
    | And (l, r) -> Printf.sprintf "And(%s, %s)" (stringify_expr l) (stringify_expr r)
    | Or (l, r) -> Printf.sprintf "Or(%s, %s)" (stringify_expr l) (stringify_expr r))
  | Grouping g -> Printf.sprintf "Grouping(%s)" (stringify_expr g)
  | LetBinding (is_rec, value_binding, rhs) -> 
    let rec_str = if is_rec then "rec " else "" in 
    let rhs_string = (match rhs with None -> "" | Some e -> " in " ^ stringify_expr e) in  
      Printf.sprintf "Let(%s %s %s)" rec_str (stringify_value_binding value_binding) rhs_string
  | IfExpr (condition, then_expr, else_expr) -> 
      let else_str = match else_expr with None -> "" | Some e -> " else " ^ stringify_expr e in
        Printf.sprintf "If(%s then %s%s)" (stringify_expr condition) (stringify_expr then_expr) else_str
  | Function (params, body) -> 
      Printf.sprintf "Fun(%s -> %s)" (stringify_patterns params) (stringify_expr body)
  | Match (expr, cases) -> 
      Printf.sprintf "PatternMatch(match %s with %s)" (stringify_expr expr) (stringify_match_cases cases)
  | FnApp (fn, args) -> 
      Printf.sprintf "FnApp(fn = %s, arg = %s)" (stringify_expr fn) (stringify_expr_list args)
  | None -> "None"
  | _ -> ""

and stringify_value_binding vb = 
  let lhs, rhs = vb.pat, vb.rhs in 
  Printf.sprintf "ValBin(%s = %s)" (stringify_pattern lhs) (stringify_expr rhs)

and stringify_params params = List.fold_left (fun acc x -> acc ^ x ^ " ") "" params

and stringify_pattern pattern = match pattern.pattern_desc with 
    ConstInteger i -> string_of_int i 
  | ConstFloat f -> string_of_float f 
  | ConstString s -> s 
  | ConstIdent i -> Printf.sprintf "Id(%s)" i 
  | True -> "true"
  | False -> "false" 
  | EmptyBrackets -> "[]"
  | EmptyParens -> "()"
  | Wildcard -> "_"

and stringify_patterns patterns = 
  List.fold_left (fun acc x -> acc ^ stringify_pattern x ^ " ") "" patterns  

and stringify_match_case case = 
  Printf.sprintf "| %s -> %s" (stringify_pattern case.lhs) (stringify_expr case.rhs)

and stringify_match_cases cases = 
  List.fold_left (fun acc case -> acc ^ (stringify_match_case case)) "" cases

let stringify_program module_items = 
  List.fold_left (fun acc x -> acc ^ (stringify_module_item x) ^ "\n") "" module_items