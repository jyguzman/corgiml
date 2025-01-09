type typ = 
  | TInt 
  | TFloat 
  | TString 
  | TBool 
  | TFunction of typ list

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
  | LetBinding of bool * string * expr * expr

  | Function of {
    param: string; 
    expr: expr;
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

and un_op = 
  | Minus of expr
  | MinusDot of expr

let rec stringify_expr expr = match expr with 
    Integer i -> string_of_int i 
  | Ident i -> Printf.sprintf "Id(%s)" i 
  | BinOp b -> (match b with 
      IAdd (l, r) -> Printf.sprintf "IAdd(%s, %s)" (stringify_expr l) (stringify_expr r) 
    | IMultiply (l, r) -> Printf.sprintf "IMultiply(%s, %s)" (stringify_expr l) (stringify_expr r) 
    | IDivide (l, r) -> Printf.sprintf "IDivide(%s, %s)" (stringify_expr l) (stringify_expr r) 
    | ISubtract (l, r) -> Printf.sprintf "ISubtract(%s, %s)" (stringify_expr l) (stringify_expr r)
    | FAdd (l, r) -> Printf.sprintf "FAdd(%s, %s)" (stringify_expr l) (stringify_expr r) 
    | FMultiply (l, r) -> Printf.sprintf "FMultiply(%s, %s)" (stringify_expr l) (stringify_expr r) 
    | FDivide (l, r) -> Printf.sprintf "FDivide(%s, %s)" (stringify_expr l) (stringify_expr r) 
    | FSubtract (l, r) -> Printf.sprintf "FSubtract(%s, %s)" (stringify_expr l) (stringify_expr r))
  | Grouping g -> stringify_expr g
  | LetBinding (is_rec, pat, l, r) -> Printf.sprintf "Let(%s%s = %s in %s)" (if is_rec then "rec " else "") pat (stringify_expr l) (stringify_expr r) 
  | IfExpr i -> Printf.sprintf "If(%s then %s else %s)" (stringify_expr i.then_cond) (stringify_expr i.then_expr) (stringify_expr i.else_expr)
  | Function f -> Printf.sprintf "Fun(%s -> %s)" f.param (stringify_expr f.expr)
  | PatternMatch pm -> Printf.sprintf "PatternMatch(match %s with %s)" (stringify_expr pm.match_expr) (stringify_match_clauses pm.clauses)
  | None | _ -> ""

and stringify_params params = List.fold_left (fun acc x -> acc ^ x ^ " ") "" params

and stringify_pattern (pat: pattern) =
  match pat with 
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

and stringify_match_clauses clauses = 
  let stringify_clause clause = 
    Printf.sprintf "| %s -> %s" (stringify_pattern clause.pattern) (stringify_expr clause.cmp_to) 
  in 
    List.fold_left (fun acc clause -> acc ^ (stringify_clause clause)) "" clauses