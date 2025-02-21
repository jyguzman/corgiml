type location = {
  line: int;
  col: int;
  start_pos: int;
  end_pos: int
}

type ty = 
  | App of string * ty list 
  | Var of string 
  | Arrow of ty * ty
  | Poly of string list * ty
  | Any (* _ (wildcard) *)

type pattern = {
  pattern_desc: pattern_desc;
  loc: location 
}

and pattern_desc = 
  | Const_integer of int 
  | Const_float of float 
  | Const_string of string 
  | Const_ident of string
  | True
  | False
  | Empty_brackets 
  | Empty_parens
  | Any (* _ *)

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
  | Grouping of expression

  | Binary of expression * string * expression
  | Unary of string * expression 

  | If of expression * expression * expression option
  | Let of bool * value_binding list * expression
  | Match of expression * case list

  | Function of string list * expression * ty option
  | Apply of expression * expression list

  | List of corgi_list
  | Tuple of expression list
  | Record of field list

and corgi_list = Nil | Cons of expression * corgi_list

and case = {
  lhs: pattern;
  rhs: expression;
}

and value_binding = {
  pat: pattern;
  rhs: expression;
  value_constraint: ty option; (* eventually need to parse type constraints *)
  location: location;
}

and field = {
  key: string;
  value: expression
}

type module_item = {
  module_item_desc: module_item_desc;
  module_item_loc: location
}

and module_item_desc =
  | Expr of expression
  | LetDeclaration of bool * value_binding list
  | TypeDefintion of string * string list * type_kind

and type_kind = 
  | Variant of constr_decl list 
  | Record of (string * ty) list 

and constr_decl = {
  name: string;
  types: ty list;
}

let int = App("Int", [])
let float = App("Float", [])
let bool = App("Bool", [])
let string = App("String", [])
let unit = App("Unit", [])

let stringify_items ?(newline = false) items stringify_item  = 
  let stringifier = (fun (curr_list_str, num_items_left) item ->
    let nl = if newline then "\n" else "" in
    let sep = if num_items_left > 1 then ", " ^ nl else "" in
    curr_list_str ^ stringify_item item ^ sep, num_items_left - 1
  ) in
  let str, _ = (List.fold_left stringifier ("", List.length items) items) in
  Printf.sprintf "%s" str

let int_list = App("List", [App("Int", [])])

let rec string_of_type = function 
  | App (name, types) -> 
    begin match name, types with 
        "Int", [] -> "Int"
      | "Float", [] -> "Float"
      | "String", [] -> "String"
      | "Bool", [] -> "Bool"
      | "Unit", [] -> "Unit"
      | "List", _ -> 
        Printf.sprintf "[%s]" (stringify_items types string_of_type)
      | _, _ -> ""
    end
  | Var name -> Printf.sprintf "Var(%s)" name 
  | Arrow (left, right) -> 
    Printf.sprintf "Arrow(%s -> %s)" (string_of_type left) (string_of_type right)
  | Poly (vars, ty) -> 
    Printf.sprintf "Poly([%s], %s)" (stringify_items vars (fun s -> s)) (string_of_type ty)
  | Any -> "Any (_)" 

and stringify_module_item mi = match mi.module_item_desc with 
    Expr e -> stringify_expr e
  | LetDeclaration (is_rec, value_bindings) ->
    let bindings_str = stringify_items value_bindings stringify_value_binding in 
      Printf.sprintf "LetDecl(%s %s)" (if is_rec then "rec " else "") bindings_str
  | TypeDefintion _ -> "need to stringify type"

and stringify_expr_list exprs = 
  stringify_items exprs stringify_expr

and stringify_expr expr = match expr.expr_desc with 
    String (bytes, _) -> Printf.sprintf "\"%s\"" (Bytes.to_string bytes)
  | Integer i -> string_of_int i 
  | Float f -> string_of_float f
  | Bool b -> string_of_bool b
  | Ident i -> Printf.sprintf "Id(%s)" i 
  | Grouping g -> Printf.sprintf "(%s)" (stringify_expr g)
  | Binary (left, op, right) -> Printf.sprintf "Binary(%s %s %s)" (stringify_expr left) op (stringify_expr right) 
  | Unary (op, expr) -> Printf.sprintf "Unary(%s%s)" op (stringify_expr expr)
  | Let (is_rec, value_bindings, rhs) -> 
    let rec_str = if is_rec then "rec " else "" in 
    let bindings_str = stringify_items value_bindings stringify_value_binding in
      Printf.sprintf "Let(%s %s %s)" rec_str bindings_str (stringify_expr rhs)
  | If (condition, then_expr, else_expr) -> 
      let else_str = match else_expr with None -> "" | Some e -> " else " ^ stringify_expr e in
        Printf.sprintf "If(%s then %s%s)" (stringify_expr condition) (stringify_expr then_expr) else_str
  | Function (params, body, _) -> 
      Printf.sprintf "Fun(%s -> %s)" (stringify_params params) (stringify_expr body)
  | Match (expr, cases) -> 
      Printf.sprintf "PatternMatch(match %s with %s)" (stringify_expr expr) (stringify_match_cases cases)
  | Apply (fn, args) -> 
      Printf.sprintf "Apply(%s, [%s])" (stringify_expr fn) (stringify_expr_list args)
  | List l -> stringify_corgi_list l
  | Tuple exprs ->
    Printf.sprintf "Tuple(%s)" (stringify_expr_list exprs)
  | Record fields -> 
    let fields_str = stringify_items fields stringify_record_field in
      Printf.sprintf "Record{%s}" fields_str

and stringify_corgi_list lst = 
  let rec unroll exprs = function
    Nil -> exprs
  | Cons (expr, rest) -> 
    unroll (expr :: exprs) rest
  in 
    let exprs = List.rev (unroll [] lst) in 
    Printf.sprintf "List[%s]" (stringify_expr_list exprs)

and stringify_value_binding vb = 
  Printf.sprintf "Binding(%s = %s)" (stringify_pattern vb.pat) (stringify_expr vb.rhs)

and stringify_params params = 
  stringify_items params (fun s -> s)

and stringify_record_field field = 
  Printf.sprintf "%s: %s" field.key (stringify_expr field.value)

and stringify_pattern pattern = match pattern.pattern_desc with 
    Const_integer i -> string_of_int i 
  | Const_float f -> string_of_float f 
  | Const_string s -> s 
  | Const_ident i -> Printf.sprintf "Id(%s)" i 
  | True -> "true"
  | False -> "false" 
  | Empty_brackets -> "[]"
  | Empty_parens -> "()"
  | Any -> "_"

and stringify_patterns patterns = 
  stringify_items patterns stringify_pattern

and stringify_match_case case = 
  Printf.sprintf "| %s -> %s" (stringify_pattern case.lhs) (stringify_expr case.rhs)

and stringify_match_cases cases = 
  stringify_items cases stringify_match_case 

let stringify_program module_items = 
  stringify_items ~newline:true module_items stringify_module_item
