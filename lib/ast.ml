type location = {
  (* start_line: int;
  start_col: int;
  end_line: int;
  end_col: int; *)
  line: int;
  col: int;
  length: int
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
  | True
  | False
  | ConstIdent of string
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

  | IfExpr of {
    then_cond: expression; 
    then_expr: expression;
    else_expr: expression option;
  }
  | LetBinding of bool * pattern * expression * expression option
  | Function of {
    param: pattern; 
    expr: expression;
  }
  | FnApp of {
    fn: expression;
    arg: expression;
  }
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

type module_item =
  | Expr of expression
  | LetDeclaration of bool * pattern * expression 
  | TypeDefintion of type_definition 

and type_definition = {
  type_name: string;
  type_constructors: typ_con list
}

and typ_con = {
  type_def_name: string;
  con_name: string;
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
  
let rec stringify_type typ = match typ with 
    TUnit -> "unit" | TInt -> "int" | TFloat -> "float" 
  | TString -> "string" | TBool -> "bool" | TNone -> "None"
  | TFunction tf -> 
    let param_str = List.fold_left (fun acc x -> acc ^ (stringify_type x) ^ "->") "(" tf.param_typs in 
      Printf.sprintf "%s) -> %s" param_str (stringify_type tf.ret_type)

let rec stringify_module_item mi = match mi with 
    Expr e -> stringify_expr e 
  | LetDeclaration (is_rec, pattern, rhs) -> 
      Printf.sprintf "LetDecl(%s%s = %s)" (if is_rec then "rec " else "") (stringify_pattern pattern) (stringify_expr rhs)
  | TypeDefintion td ->
    let type_cons = List.fold_left (fun acc x -> acc ^ "|" ^ (stringify_type_con x)) "" td.type_constructors in
      Printf.sprintf "TypeDefinition(%s = %s)" td.type_name type_cons

and stringify_type_con v = 
  let typ_string = match v.of_type with None -> "" | Some t -> " of " ^ stringify_type t in 
    Printf.sprintf "%s%s" v.con_name typ_string  

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
  | LetBinding (is_rec, pat, lhs, rhs) -> 
    let rec_str = if is_rec then "rec " else "" in 
    let rhs_string = (match rhs with None -> "" | Some e -> " in " ^ stringify_expr e) in  
      Printf.sprintf "Let(%s%s = %s%s)" rec_str (stringify_pattern pat) (stringify_expr lhs) rhs_string
  | IfExpr i -> 
      let else_str = match i.else_expr with None -> "" | Some e -> " else " ^ stringify_expr e in
        Printf.sprintf "If(%s then %s%s)" (stringify_expr i.then_cond) (stringify_expr i.then_expr) else_str
  | Function f -> 
      Printf.sprintf "Fun(%s -> %s)" (stringify_pattern f.param) (stringify_expr f.expr)
  | Match (expr, cases) -> 
      Printf.sprintf "PatternMatch(match %s with %s)" (stringify_expr expr) (stringify_match_cases cases)
  | FnApp fa -> 
      Printf.sprintf "FnApp(fn = %s, arg = %s)" (stringify_expr fa.fn) (stringify_expr fa.arg)
  | None -> "None"
  | _ -> ""

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

and stringify_match_case case = 
  Printf.sprintf "| %s -> %s" (stringify_pattern case.lhs) (stringify_expr case.rhs)

and stringify_match_cases cases = 
  List.fold_left (fun acc case -> acc ^ (stringify_match_case case)) "" cases

let stringify_program module_items = 
  List.fold_left (fun acc x -> acc ^ (stringify_module_item x) ^ "\n") "" module_items