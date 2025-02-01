(* open Type *)

type err = 
  | Type_mismatch of string

let src_line source_info line_num = 
  List.nth source_info.Lexer.lines line_num

let src_line_from_pos source_info token_start_pos = 
  Hashtbl.find source_info.Lexer.map token_start_pos

let expr_src_len expr = 
  expr.Ast.loc.end_pos - expr.loc.start_pos

let expr_src_span source_info expr = 
  String.sub source_info.Lexer.raw expr.Ast.loc.start_pos (expr_src_len expr)

let expr_src_lines source_info expr = 
  let span = expr_src_span source_info expr in
    String.split_on_char '\n' span

let expr_num_lines source_info expr = 
  List.length (expr_src_lines source_info expr)

let expr_src_lines source_info expr = 
  let lines = expr_src_lines source_info expr in 
  let line_nums = List.init (List.length lines) (fun i -> i + expr.loc.line) in 
  List.map (fun line_num -> (line_num, List.nth source_info.lines line_num)) line_nums


module type FORMATTER = sig 
  val display_expr: Ast.expression -> string
end

module Formatter (S: sig val src: Lexer.source end) : FORMATTER = struct 
  (* let red str = "\027[31m" ^ str ^ "\027[0m" *)

  let display_expr expr = 
    let expr_lines = expr_src_lines S.src expr in
    List.fold_left (fun acc (line_num, line) -> acc ^ Printf.sprintf "%d |   %s\n"  (line_num + 1) line) "" expr_lines

  (* let underline expr =
    let line_num, start_pos = expr.Ast.loc.line, expr.loc.col in 
    let length = expr.loc.end_pos - expr.loc.start_pos + 1 in    
    let src_line = List.nth src_lines line_num in
    let whitespace = String.make (start_pos) ' ' in 
    let underline = whitespace ^ red (String.make length '^') in 
      Printf.sprintf "%s\n%s" src_line underline  *)
end 
 
let get_type_str = function 
  Ast.Literal Integer _ -> "int"
| Ast.Literal Float _ -> "float"
| Ast.Literal String _ -> "string"
| Ast.Literal Bool _ -> "bool" 
| _ -> ""

let bin_op_mismatch_template = format_of_string "The (%s) operator works only with %s values."


let int_bin_op_error_str op l r =  
  let word = match op with
    "+" -> "add" | "*" -> "multiply" | "-" -> "subtract" | "/" -> "divide" | _ -> ""
  in
  let float_hint = Printf.sprintf "\n\nHint: To %s floats, use (%s.)" word op in
  let l_str, r_str = Ast.stringify_expr l, Ast.stringify_expr r in 
  let template = Printf.sprintf bin_op_mismatch_template op "integer" in
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
  
let float_bin_op_error_str op l r =  
  let l_str, r_str = Ast.stringify_expr l, Ast.stringify_expr r in 
  let template = Printf.sprintf bin_op_mismatch_template op "float" in
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
  
let compare_error_str op l r =  
  let l_str, r_str = Ast.stringify_expr l, Ast.stringify_expr r in 
  let template = Printf.sprintf "The (%s) operator works only with values of the same type." op in
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
  
let logical_error_str op _l _r =  
  (* let l_str, r_str = Ast.stringify_expr l, Ast.stringify_expr r in  *)
  Printf.sprintf bin_op_mismatch_template op "boolean"

