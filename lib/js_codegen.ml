open Ast

let (let*) r f = match r with 
  Ok v -> f v 
| Error e -> Error e

let js_of_pattern pat = 
  match pat.pattern_desc with 
      Empty_parens -> Ok "undefined" (* need to change this *)
    | Const_integer i -> Ok (string_of_int i)
    | Const_float f -> Ok (Printf.sprintf "%0.2f" f)
    | Const_ident i -> Ok i
    | True -> Ok "true"
    | False -> Ok "false"
    | _ -> Error ("not a Corgi pattern")

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

    | If (condition, then_, _else_) -> 
      let* condition = js_of_corgi_expr condition in 
      let* then_ = js_of_corgi_expr then_ in
      Ok (Printf.sprintf "%s ? %s : " condition then_)

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

    | Record_access (expr, name) -> 
      let* expr = js_of_corgi_expr expr in 
      Ok (Printf.sprintf "%s[\"%s\"]" expr name) 

    | _ -> Error ("not a Corgi expr")

and js_of_field field = 
  let* value = js_of_corgi_expr field.value in 
  Ok (Printf.sprintf "%s: %s" field.key value)

and js_of_fields fields = 
  let rec js_of_fields_aux acc fields = 
    if List.length fields = 0 then 
      Ok acc 
    else
      let* js_pair = js_of_field @@ List.hd fields in 
      js_of_fields_aux (js_pair :: acc) (List.tl fields)
  in
    let* pairs = js_of_fields_aux [] fields in 
    Ok (String.concat ", " (List.rev pairs))

and js_of_expr_list exprs = 
  let rec js_of_expr_list_aux acc exprs = 
    if List.length exprs = 0 then 
      Ok acc 
    else
      let* js_pair = js_of_corgi_expr @@ List.hd exprs in 
      js_of_expr_list_aux (js_pair :: acc) (List.tl exprs)
  in
    let* pairs = js_of_expr_list_aux [] exprs in 
    Ok (String.concat ", " (List.rev pairs))

let js_of_value_binding vb = 
  let pat, expr = vb.pat, vb.rhs in 
  let* js_pat = js_of_pattern pat in 
  let* js_expr = js_of_corgi_expr expr in 
  Ok (Printf.sprintf "const %s = %s;" js_pat js_expr)

let js_of_value_bindings value_bindings = 
  let rec js_of_value_bindings_aux acc value_bindings = 
    if List.length value_bindings = 0 then 
      Ok acc 
    else
      let* js_assignment = js_of_value_binding @@ List.hd value_bindings in 
      js_of_value_bindings_aux (js_assignment :: acc) (List.tl value_bindings)
  in
    let* assignments = js_of_value_bindings_aux [] value_bindings in 
    Ok (String.concat "\n" assignments)

let js_of_corgi_mi mi =
  match mi.module_item_desc with 
      Expr e -> js_of_corgi_expr e
    | LetDeclaration (_, value_bindings) -> js_of_value_bindings value_bindings
    | _ -> Error "module item type doesn't exist"

let js_of_corgi_program prog = 
  let rec js_corgi_program_aux acc items = 
    if List.length items = 0 then 
      Ok acc 
    else
      let* js_assignment = js_of_corgi_mi @@ List.hd items in 
      js_corgi_program_aux (js_assignment :: acc) (List.tl items)
  in
    let* assignments = js_corgi_program_aux [] prog in 
    Ok (String.concat "\n" (List.rev assignments))

module Codegen = struct 
  let gen_js_expr corgi_expr = js_of_corgi_expr corgi_expr
  let js_of_mi mi = js_of_corgi_mi mi
end
