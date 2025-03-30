open Ast

let (let*) r f = match r with 
  Ok v -> f v 
| Error e -> Error e

let js_of_list gen_js_func items separator = 
  let rec js_of_pat_list_aux acc items = 
    if List.length items = 0 then 
      Ok acc 
    else
      let* js_pair = gen_js_func (List.hd items) in 
      js_of_pat_list_aux (js_pair :: acc) (List.tl items)
  in
    let* item_strs = js_of_pat_list_aux [] items in 
    Ok (String.concat separator (List.rev item_strs))

let js_of_pattern pat = 
  match pat.pattern_desc with 
      Empty_parens -> Ok "undefined" (* need to change this *)
    | Const_integer i -> Ok (string_of_int i)
    | Const_float f -> Ok (Printf.sprintf "%0.2f" f)
    | Const_ident i -> Ok i
    | True -> Ok "true"
    | False -> Ok "false"
    | _ -> Error ("not a Corgi pattern")

let js_of_pat_list pats = 
  js_of_list js_of_pattern pats ", "

let rec js_of_corgi_expr expr = 
  match expr.expr_desc with 
      Integer i -> Ok (string_of_int i) 
    | Float f -> Ok (Printf.sprintf "%0.2f" f)
    | Bool b -> Ok (string_of_bool b)
    | Ident i | UpperIdent i -> Ok i
    | String (bytes, _) -> Ok (Printf.sprintf "\"%s\"" (String.of_bytes bytes))
    | Grouping g -> 
      let* inner = js_of_corgi_expr g in 
      Ok (Printf.sprintf "(%s)" inner)

    | Binary (left, (op, _), right) -> 
      let* left = js_of_corgi_expr left in 
      let* right = js_of_corgi_expr right in 
      Ok (Printf.sprintf "%s %s %s" left op right)

    | Unary ((op, _), operand) -> 
      let* operand = js_of_corgi_expr operand in 
      Ok (Printf.sprintf "%s%s" op operand)

    | Function (params, body, _) -> 
      let params = stringify_params params in
      let* body = js_of_corgi_expr body in
      Ok (Printf.sprintf "(%s) => %s" params body)
  
    | Apply (fn, args) ->
      let* fn = js_of_corgi_expr fn in 
      let* args = js_of_expr_list args in 
      Ok (Printf.sprintf "%s(%s)" fn args)

    | If (condition, then_, else_) -> 
      let* condition = js_of_corgi_expr condition in 
      let* then_ = js_of_corgi_expr then_ in
      let* else_ = js_of_corgi_expr else_ in
      Ok (Printf.sprintf "%s ? %s : %s" condition then_ else_)

    | Let (_is_rec, value_bindings, body) -> 
      let pats = List.map (fun vb -> vb.pat) value_bindings in
      let* js_pats = js_of_pat_list pats in
      let values = List.map (fun vb -> vb.rhs) value_bindings in 
      let* js_values = js_of_expr_list values in 
      let* body = js_of_corgi_expr body in 
      Ok (Printf.sprintf "((%s) => %s)(%s)" js_pats body js_values)

    | Tuple elems -> 
      let* exprs = js_of_expr_list elems in 
      Ok (Printf.sprintf "[%s]" exprs)

    | List list -> (* use a JS runtime linked list instead *)
      let exprs = unroll_corgi_list [] list in 
      let* exprs = js_of_expr_list exprs in 
      Ok (Printf.sprintf "[%s]" exprs) 

    | Record fields ->  
      let* pairs = js_of_fields fields in
      Ok (Printf.sprintf "{%s}" pairs)

    | Field_access (expr, name) -> 
      let* expr = js_of_corgi_expr expr in 
      Ok (Printf.sprintf "%s[\"%s\"]" expr name) 

    | _ -> Error ("not a Corgi expr")

and js_of_field field = 
  let* value = js_of_corgi_expr field.value in 
  Ok (Printf.sprintf "%s: %s" field.key value)

and js_of_fields fields = 
  js_of_list js_of_field fields ", "

and js_of_expr_list exprs = 
  js_of_list js_of_corgi_expr exprs ", "

let js_of_value_binding vb = 
  let pat, expr = vb.pat, vb.rhs in 
  let* js_pat = js_of_pattern pat in 
  let* js_expr = js_of_corgi_expr expr in 
  Ok (Printf.sprintf "const %s = %s;" js_pat js_expr)

let js_of_value_bindings value_bindings = 
  js_of_list js_of_value_binding value_bindings "\n"

let js_of_corgi_mi mi =
  match mi.module_item_desc with 
      Expr e -> js_of_corgi_expr e
    | LetDeclaration (_, value_bindings) -> js_of_value_bindings value_bindings
    | _ -> Error "module item type doesn't exist"

let js_of_corgi_program prog = 
  js_of_list js_of_corgi_mi prog "\n"

module Codegen = struct 
  let gen_js_expr corgi_expr = js_of_corgi_expr corgi_expr
  let js_of_mi mi = js_of_corgi_mi mi
end
