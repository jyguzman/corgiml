type loc = {
  line: int;
  col: int;
  length: int;
}

type corgi_string = {
  bytes: bytes;
  len: int
}

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


type literal = 
  | Integer of int * loc
  | Float of float * loc
  | String of string * loc
  | Bool of bool * loc

type expr = 
  | None
  (* | Integer of int * loc
  | Float of float * loc *)
  | Literal of literal
  (* | String of string  *)
  | CorgiString of corgi_string
  | Bool of bool
  | Ident of string 
  | BinOp of bin_op 
  | UnOp of un_op 
  | Grouping of expr

  | IfExpr of {
    then_cond: expr; 
    then_expr: expr;
    else_expr: expr option;
  }
  | LetBinding of bool * pattern * expr * expr option
  | Function of {
    param: pattern; 
    expr: expr;
  }
  | FnApp of {
    fn: expr;
    arg: expr;
  }

  | PatternMatch of expr * case list

  and case = {
    lhs: pattern;
    rhs: expr;
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
  | LetDecl of bool * pattern * expr 
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


let op = function  
    IAdd (_, _) -> "+"
  | IMultiply (_, _) -> "*" 
  | ISubtract (_, _) -> "-"
  | IDivide (_, _) -> "/"
  | FAdd (_, _) -> "+."
  | FMultiply (_, _) -> "*." 
  | FSubtract (_, _) -> "-."
  | FDivide (_, _) -> "/."
  | Less (_, _) -> "<"
  | Leq (_, _) -> "<="
  | Greater (_, _) -> ">"
  | Geq (_, _) -> ">="
  
let rec stringify_type typ = match typ with 
    TUnit -> "unit" | TInt -> "int" | TFloat -> "float" 
  | TString -> "string" | TBool -> "bool" | TNone -> "None"
  | TFunction tf -> 
    let param_str = List.fold_left (fun acc x -> acc ^ (stringify_type x) ^ "->") "(" tf.param_typs in 
      Printf.sprintf "%s) -> %s" param_str (stringify_type tf.ret_type)

let rec stringify_module_item mi = match mi with 
  Expr e -> stringify_expr e 
  | LetDecl (is_rec, pattern, rhs) -> 
      Printf.sprintf "LetDecl(%s%s = %s)" (if is_rec then "rec " else "") (stringify_pattern pattern) (stringify_expr rhs)
  | TypeDefintion td ->
    let type_cons = List.fold_left (fun acc x -> acc ^ "|" ^ (stringify_type_con x)) "" td.type_constructors in
      Printf.sprintf "TypeDefinition(%s = %s)" td.type_name type_cons

and stringify_type_con v = 
  let typ_string = match v.of_type with None -> "" | Some t -> " of " ^ stringify_type t in 
    Printf.sprintf "%s%s" v.con_name typ_string  

and stringify_expr_list exprs = 
  List.fold_left (fun acc expr -> acc ^ (stringify_expr expr) ^ ",") "" exprs 

and stringify_expr expr = match expr with 
    Literal String (s, _) -> Printf.sprintf "\"%s\"" s
  | Literal Integer (i, _) -> string_of_int i 
  | Literal Float (f, _) -> string_of_float f
  | Literal Bool (b, _) -> string_of_bool b
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
      Printf.sprintf "Let(%s%s = %s%s)" rec_str (stringify_pattern pat) (stringify_expr l) in_string
  | IfExpr i -> 
      let else_str = match i.else_expr with None -> "" | Some e -> " else " ^ stringify_expr e in
      Printf.sprintf "If(%s then %s%s)" (stringify_expr i.then_cond) (stringify_expr i.then_expr) else_str
  | Function f -> 
      Printf.sprintf "Fun(%s -> %s)" (stringify_pattern f.param) (stringify_expr f.expr)
  | PatternMatch (expr, cases) -> 
      Printf.sprintf "PatternMatch(match %s with %s)" (stringify_expr expr) (stringify_match_cases cases)
  | FnApp fa -> 
      Printf.sprintf "FnApp(fn = %s, arg = %s)" (stringify_expr fa.fn) (stringify_expr fa.arg)
  | None -> "None"
  | _ -> ""

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


and stringify_match_case case = 
  Printf.sprintf "| %s -> %s" (stringify_pattern case.lhs) (stringify_expr case.rhs)

and stringify_match_cases cases = 
  List.fold_left (fun acc case -> acc ^ (stringify_match_case case)) "" cases

let stringify_program module_items = 
  List.fold_left (fun acc x -> acc ^ (stringify_module_item x) ^ "\n") "" module_items