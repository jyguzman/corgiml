type err = 
  | Type_mismatch of Ast.expression * string
  
module Formatter(Src: sig val source: string list end) = struct 
  let format_line line_num = 
    Printf.sprintf "%d|\t%s" line_num (List.nth Src.source line_num)

  let red str = "\027[31m" ^ str ^ "\027[0m"

  let format_expr expr = 
    let line_num, start_pos, length = expr.Ast.loc.line, expr.loc.col, expr.loc.length in
    let src_line = List.nth Src.source line_num in
      String.sub src_line start_pos length

  let underline expr =
    let line_num, start_pos, length = expr.Ast.loc.line, expr.loc.col, expr.loc.length in
    let src_line = List.nth Src.source line_num in
    let whitespace = String.make (start_pos) ' ' in 
    let underline = whitespace ^ red (String.make length '^') in 
      Printf.sprintf "%s\n%s" src_line underline 
end 
 

let get_type_str = function 
  Ast.Literal Integer _ -> "int"
| Ast.Literal Float _ -> "float"
| Ast.Literal String _ -> "string"
| Ast.Literal Bool _ -> "bool" 
| _ -> ""

let bin_op_mismatch_template = format_of_string {|
  "%s" expects two %ss, but it got the following: 
      left: %s (type: %s)
      right: %s (type: %s)
|}

let int_bin_op_error_str bin_op l r =  
  let op = Ast.op_for bin_op in 
  let word = match op with
    "+" -> "add" | "*" -> "multiply" | "-" -> "subtract" | "/" -> "divide" | _ -> ""
  in
  let float_hint = Printf.sprintf "\n\nHint: To %s floats, use \"%s.\"" word op in
  let l_str, r_str = Ast.stringify_expr l, Ast.stringify_expr r in 
  (* let l_str, r_str = ., Ast.stringify_expr r in  *)
  let template = Printf.sprintf bin_op_mismatch_template op "integer" l_str (get_type_str l.expr_desc) r_str (get_type_str r.expr_desc) in
  let hint = match l.expr_desc, r.expr_desc with 
    | Ast.Literal Float _, Ast.Literal Float _ -> float_hint
    | Ast.Literal String _, Ast.Literal String _ -> 
      if op = "+" then "\n\nHint: To concatenate strings, use '^'"  else ""
    | Ast.Literal Float _, Ast.Literal Integer _ -> 
      let expr_str = if String.contains r_str ' '  then Printf.sprintf "(%s)" r_str else r_str in
      Printf.sprintf "%s\nHint: You can convert an int to a float with \"float_of_int (%s)\"" float_hint expr_str
    | Ast.Literal Integer _, Ast.Literal Float _ -> 
      let expr_str = if String.contains l_str ' '  then Printf.sprintf "(%s)" l_str else l_str in
      Printf.sprintf "%s\nHint: You can convert an int to a float with \"float_of_int %s\"" float_hint expr_str
    | _, _ -> ""
  in 
    template ^ hint
  
let float_bin_op_error_str bin_op l r =  
  let op = Ast.op_for bin_op in
  let l_str, r_str = Ast.stringify_expr l, Ast.stringify_expr r in 
  let template = Printf.sprintf bin_op_mismatch_template op "float" l_str (get_type_str l.expr_desc) r_str (get_type_str r.expr_desc) in
  let hint = match l.expr_desc, r.expr_desc with 
    | Ast.Literal String _, Ast.Literal String _ -> 
      if op = "+." then "\n\nHint: To concatenate strings, use '^'"  else ""
    | Ast.Literal Float _, Ast.Literal Integer _ -> 
      let expr_str = if String.contains r_str ' '  then Printf.sprintf "(%s)" r_str else r_str in
      Printf.sprintf "\nHint: You can convert an int to a float with \"float_of_int (%s)\"" expr_str
    | Ast.Literal Integer _, Ast.Literal Float _ -> 
      let expr_str = if String.contains l_str ' '  then Printf.sprintf "(%s)" l_str else l_str in
      Printf.sprintf "\nHint: You can convert an int to a float with \"float_of_int %s\"" expr_str
    | _, _ -> ""
  in 
    template ^ hint
  
let compare_error_str bin_op l r =  
  let op = Ast.op_for bin_op in
  let l_str, r_str = Ast.stringify_expr l, Ast.stringify_expr r in 
  let template = Printf.sprintf bin_op_mismatch_template op "boolean" l_str (get_type_str l.expr_desc) r_str (get_type_str r.expr_desc) in
  let hint = match l.expr_desc, r.expr_desc with 
    | Ast.Literal Float _, Ast.Literal Integer _ -> 
      let expr_str = if String.contains r_str ' '  then Printf.sprintf "(%s)" r_str else r_str in
      Printf.sprintf "\nHint: You can convert an int to a float with float_of_int (%s)" expr_str
    | Ast.Literal Integer _, Ast.Literal Float _ -> 
      let expr_str = if String.contains l_str ' '  then Printf.sprintf "(%s)" l_str else l_str in
      Printf.sprintf "\nHint: You can convert an int to a float with float_of_int (%s)" expr_str
    | _, _ -> ""
  in 
    template ^ hint
  
let logical_error_str bin_op l r =  
  let op = Ast.op_for bin_op in
  let l_str, r_str = Ast.stringify_expr l, Ast.stringify_expr r in 
  Printf.sprintf bin_op_mismatch_template op "boolean" l_str (get_type_str l.expr_desc) r_str (get_type_str r.expr_desc)

