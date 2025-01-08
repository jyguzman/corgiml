type typ = 
  | TInt 
  | TFloat 
  | TString 
  | TBool 
  | TFunction of typ list

type constant = 
  | Integer of int 
  | Float of float 
  | String of string 
  | Bool of bool
  | EmptyBrackets 
  | EmptyParens


type pattern = 
  | Constant of constant
  | Ident of string


type expr = 
  | None
  | Pattern of pattern
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
    pattern: string;
    cmp_to: expr;
  }
    

and bin_op = 
  | Add of expr * expr 
  | Multiply of expr * expr 
  | Subtract of expr * expr 
  | Divide of expr * expr

and un_op = 
  | Minus of expr


let rec stringify_expr expr = match expr with 
    Integer i -> string_of_int i 
  | Ident i -> Printf.sprintf "Id(%s)" i 
  | BinOp b -> (match b with 
      Add (l, r) -> Printf.sprintf "Add(%s, %s)" (stringify_expr l) (stringify_expr r) 
    | Multiply (l, r) -> Printf.sprintf "Multiply(%s, %s)" (stringify_expr l) (stringify_expr r) 
    | Divide (l, r) -> Printf.sprintf "Divide(%s, %s)" (stringify_expr l) (stringify_expr r) 
    | Subtract (l, r) -> Printf.sprintf "Subtract(%s, %s)" (stringify_expr l) (stringify_expr r))
  | Grouping g -> stringify_expr g
  | LetBinding (is_rec, pat, l, r) -> Printf.sprintf "Let(%s%s = %s in %s)" (if is_rec then "rec " else "") pat (stringify_expr l) (stringify_expr r) 
  | IfExpr i -> Printf.sprintf "If(%s then %s)" (stringify_expr i.then_cond) (stringify_expr i.then_expr)
  | Function f -> Printf.sprintf "Fun(%s -> %s)" f.param (stringify_expr f.expr)
  | PatternMatch pm -> Printf.sprintf "PatternMatch(match %s with %s)" (stringify_expr pm.match_expr) (stringify_match_clauses pm.clauses)
  | _ -> ""

and stringify_params params = List.fold_left (fun acc x -> acc ^ x ^ " ") "" params
and stringify_match_clauses clauses = 
  let stringify_clause clause = 
    Printf.sprintf "| %s -> %s" clause.pattern (stringify_expr clause.cmp_to) 
  in 
    List.fold_left (fun acc clause -> acc ^ (stringify_clause clause)) "" clauses