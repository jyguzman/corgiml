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

and bin_op = 
  | Add of expr * expr 
  | Multiply of expr * expr 
  | Subtract of expr * expr 
  | Divide of expr * expr

and un_op = 
  | Minus of expr


let stringify_params params = 
  List.fold_left (fun acc x -> acc ^ x ^ " ") "" params

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
  | _ -> ""