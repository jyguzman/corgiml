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
  | Integer of int
  | Float of float
  | Bool of bool
  | String of bytes * int
  | Ident of string 
  | BinOp of expression * string * expression
  | UnOp of string * expression 
  | Grouping of expression
  | If of expression * expression * expression option
  | Let of bool * value_binding * expression option
  | Function of pattern list * expression
  | Apply of expression * expression list
  | Match of expression * case list

and case = {
  lhs: pattern;
  rhs: expression;
}

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
    String (bytes, _) -> Printf.sprintf "\"%s\"" (Bytes.to_string bytes)
  | Integer i -> string_of_int i 
  | Float f -> string_of_float f
  | Bool b -> string_of_bool b
  | Ident i -> Printf.sprintf "Id(%s)" i 
  | BinOp (left, op, right) -> Printf.sprintf "BinOp(%s %s %s)" (stringify_expr left) op (stringify_expr right) 
  | UnOp (op, expr) -> Printf.sprintf "UnOp(%s%s)" op (stringify_expr expr)
  | Let (is_rec, value_binding, rhs) -> 
    let rec_str = if is_rec then "rec " else "" in 
    let rhs_string = (match rhs with None -> "" | Some e -> " in " ^ stringify_expr e) in  
      Printf.sprintf "Let(%s %s %s)" rec_str (stringify_value_binding value_binding) rhs_string
  | If (condition, then_expr, else_expr) -> 
      let else_str = match else_expr with None -> "" | Some e -> " else " ^ stringify_expr e in
        Printf.sprintf "If(%s then %s%s)" (stringify_expr condition) (stringify_expr then_expr) else_str
  | Function (params, body) -> 
      Printf.sprintf "Fun(%s -> %s)" (stringify_patterns params) (stringify_expr body)
  | Match (expr, cases) -> 
      Printf.sprintf "PatternMatch(match %s with %s)" (stringify_expr expr) (stringify_match_cases cases)
  | Apply (fn, args) -> 
      Printf.sprintf "App(%s, [%s])" (stringify_expr fn) (stringify_expr_list args)
  | _ -> "INVALID EXPR"

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