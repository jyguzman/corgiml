open Token
open Ast

type parse_error = 
  | Unexpected_eof of string
  | Unexpected_token of string
  | Invalid_rec_let_binding of string
  | Unsupported_type of string

let (let*) r f = match r with 
  Ok v -> f v 
| Error e -> Error e 

module type PARSER = sig 
  val parse: unit -> (token, parse_error) result
end

module type TOKEN_STREAM = sig 
  val init: token list -> unit
  val peek: unit -> token option 
  val pos: unit -> (int, parse_error) result
  val advance: unit -> unit 
  val prev: unit -> token
  val expect: string -> (token, parse_error) result
  val accept: string -> bool
  val accept_any: string list -> bool
  val take: unit -> (token, parse_error) result
  val eof: unit -> bool
end

let loc token = 
  let len = String.length token.lexeme in 
  {col = token.col; line = token.line; start_pos = token.pos; end_pos = token.pos + len - 1}

module TokenStream : TOKEN_STREAM = struct 
  let tokens = ref []
  let prev = ref (Token.make "" EOF "" (-1) (-1) (-1))
  
  let init toks = tokens := toks 

  let peek () = match !tokens with 
    [] -> None 
  | x :: _ -> Some x

  let advance () = match !tokens with 
    [] -> () 
  | x :: xs ->
    let _ = tokens := xs in prev := x

  let prev () = !prev

  let take () = match !tokens with 
    [] -> Error (Unexpected_eof "unexpected end of file")  
  | x :: _ -> Ok x

  let pos () = match !tokens with 
    [] -> Error (Unexpected_eof "unexpected end of file")  
  | x :: _ -> Ok x.pos

  let expect (lexeme_or_name: string) = match !tokens with 
    [] -> Error (Unexpected_token "unexpected end of file")
  | x :: _ -> 
    if x.lexeme = lexeme_or_name || x.name = lexeme_or_name then 
      let _ = advance () in Ok (x)
    else 
      Error (Unexpected_token ("expected " ^ lexeme_or_name ^ " but got " ^ stringify_token x))

  let accept lexeme_or_name = match !tokens with 
    [] -> false
  | x :: _ -> 
    if x.lexeme = lexeme_or_name || x.name = lexeme_or_name then 
      let _ = advance () in true 
    else 
      false

  let accept_any strings = match !tokens with 
    [] -> false
  | x :: _ -> List.mem x.lexeme strings || List.mem x.name strings 

  let eof () = match !tokens with 
    | [] | [_] -> true 
    | _ -> false
end

type handler = 
  | Nud of (unit -> (expression, parse_error) result) 
  | Led of (expression -> (expression, parse_error) result) 


module Parser (Stream : TOKEN_STREAM) = struct  
  let prec_table = Hashtbl.create 64
  let bp_table = Hashtbl.create 64

  let set_bp op_lexeme bp = Hashtbl.add bp_table op_lexeme bp

  let _ = set_bp "if" 5
  let _ = List.iter (fun op -> set_bp op 10) ["<"; "<="; ">"; ">="; "="]
  let _ = List.iter (fun op -> set_bp op 20) ["+"; "+."; "-"; "-."]
  let _ = List.iter (fun op -> set_bp op 30) ["*"; "*."; "/"; "/."]
  let _ = List.iter (fun t -> set_bp t 0) ["fun"; "let"; "match"] 
  let _ = List.iter (fun t -> set_bp t 0) [")"; "->"; "|"; "of"; "in"; "type"; "with"; "then"; "else"; ";;"; "eof"]

  let lbp token =   
    match Hashtbl.find_opt bp_table token.lexeme with 
      Some bp -> bp
    | None -> 70 (* assume function application if we can't find a BP *)  

  let expr_node expr_desc location = 
    {expr_desc = expr_desc; loc = location}

  let nud token = 
    let loc = loc token in 
    match token.token_type with 
      Literal l -> (match l with 
          Integer i -> Ok (expr_node (Integer i) loc)
        | Decimal d -> Ok (expr_node (Float d) loc)
        | String s -> let bytes = String.to_bytes s in Ok (expr_node (String (bytes, Bytes.length bytes)) loc) 
        | Ident i -> Ok (expr_node (Ident i) loc))
    | Keywords True -> Ok (expr_node (Bool true) loc)
    | Keywords False -> Ok (expr_node (Bool false) loc)
    | _ ->  
      try match Hashtbl.find prec_table token.lexeme with 
        Nud nud -> nud ()
      | Led _ -> Error (Unexpected_token ("Unexpected token for start of expression: " ^ stringify_token token))
      with _ -> 
        Error (Unexpected_token ("expected the start of an expression (literal, identifier, prefix operator, or opening delimiter) but got " ^ stringify_token token))

  let led left token = 
    match Hashtbl.find_opt prec_table token.lexeme with 
      None -> Error (Unexpected_token ("unexpected token " ^ stringify_token token))
    | Some handler -> (match handler with 
        Led led -> led left
      | Nud _ -> Error (Unexpected_token ("expected an infix operator or token but got " ^ stringify_token token)))

  let rec expr () = 
    parse_expr 0

  and parse_expr rbp =
    let rec parse_expr_aux left =
      let* curr = Stream.take () in
      let lbp = lbp curr in if rbp >= lbp then 
        Ok left
      else
        let* left = if lbp = 70 then 
          parse_function_application left
        else 
          let _ = Stream.advance () in 
            led left curr 
        in
          parse_expr_aux left
    in 
      let* curr = Stream.take () in 
      let _ = Stream.advance () in
      let* left = nud curr in
        parse_expr_aux left

  and parse_function_application left = 
    let rec parse_fn_app_aux args =
      let* curr = Stream.take () in
      match Hashtbl.find_opt bp_table curr.lexeme with
        Some _ -> Ok args 
      | None -> let* arg = parse_expr 71 in 
          parse_fn_app_aux (arg :: args)
    in 
    let* args = parse_fn_app_aux [] in 
    let loc = {
      line = left.loc.line;
      col = left.loc.col;
      start_pos = left.loc.start_pos;
      end_pos = (List.hd args).loc.end_pos
    } in
    Ok (expr_node (Apply (left, List.rev args)) (loc))


  let parse_grouped () = 
    let lparen = Stream.prev () in 
    let* inner = expr () in
    let* rparen = Stream.expect ")" in
    let location = {
      line = lparen.line; 
      col = lparen.col; 
      start_pos = lparen.pos; 
      end_pos = rparen.pos
    } in 
      Ok (expr_node (Grouping inner) location)

  let parse_pattern () = 
    let* next = Stream.take () in 
    let loc = loc next in 
    match next.token_type with  
      Literal Integer i -> Ok {pattern_desc = ConstInteger i; loc = loc}
    | Literal Decimal d -> Ok {pattern_desc = ConstFloat d; loc = loc}
    | Literal String s -> Ok {pattern_desc = ConstString s; loc = loc}
    | Literal Ident i -> Ok {pattern_desc = ConstIdent i; loc = loc}
    | Keywords True -> Ok {pattern_desc = True; loc = loc}
    | Keywords False -> Ok {pattern_desc = False; loc = loc}
    | Special EmptyParens -> Ok {pattern_desc = EmptyParens; loc = loc}
    | Special Wildcard -> Ok {pattern_desc = Wildcard; loc = loc}
    | _ -> Error (Unexpected_token ("expected a pattern but got " ^ (stringify_token next)))

  let parse_patterns () = 
    let rec parse_patterns_aux patterns = 
      if Stream.accept_any ["ident"; "true"; "false"; "()"; "_"] then 
        let* pattern = parse_pattern () in
        let _ = Stream.advance () in
          parse_patterns_aux (pattern :: patterns)
      else 
        Ok (List.rev patterns)
    in 
      parse_patterns_aux [] 

  (* let make_fn_node params body loc = 
    let params = List.rev params in 
      List.fold_left (fun fn param -> expr_node (Function {param = param; expr = fn}) loc) body params *)

  let parse_function () =
    let fun_token = Stream.prev () in
    let* patterns = parse_patterns () in 
    let* _ = Stream.expect "->" in
    let* body = expr () in
    let* pos = Stream.pos () in 
    let location = {
      line = fun_token.line; 
      col = fun_token.col; 
      start_pos = fun_token.pos; 
      end_pos = pos
    } in 
      Ok (expr_node (Function (patterns, body, None)) location)

  let parse_value_binding let_loc =
    let* idents = parse_patterns () in
    let num_idents = List.length idents in 
    let lhs = List.hd idents in
    let* _ = Stream.expect "=" in 
    let* rhs = if num_idents = 1 then 
      expr () 
    else 
      let* body = expr () in
      let* curr = Stream.take () in 
      let location = {line = let_loc.line; col = let_loc.col; start_pos = let_loc.start_pos; end_pos = curr.pos} in  
        Ok (expr_node (Function (List.tl idents, body, None)) location)
    in 
    let* pos = Stream.pos () in
    let location = {line = let_loc.line; col = let_loc.col; start_pos = let_loc.start_pos; end_pos = pos} in  
      Ok {pat = lhs; rhs = rhs; val_constraint = None; location = location}   

  let parse_let_binding () = 
    let let_tok = Stream.prev () in 
    let is_rec = Stream.accept ("rec") in 
    let* value_binding = parse_value_binding (loc let_tok) in
    let* body = if Stream.accept ("in") then
      let* expr = expr () in Ok (Some expr)
    else 
      Ok None 
    in 
    let* curr = Stream.take () in 
    let location = {
      line = let_tok.line; 
      col = let_tok.col; 
      start_pos = let_tok.pos; 
      end_pos = curr.pos} 
    in  
      Ok (expr_node (Let (is_rec, value_binding, body)) location)

  let parse_if_expr () = 
    let if_tok = Stream.prev () in
    let* condition = expr () in 
    let* _ = Stream.expect "then" in 
    let* then_expr = expr () in 
    let* else_expr = if Stream.accept "else" then 
      let* expr = expr () in Ok (Some expr)
    else 
      Ok None in  
    let* pos = Stream.pos () in 
    let location = {
      line = if_tok.line; 
      col = if_tok.col; 
      start_pos = if_tok.pos; 
      end_pos = pos
    } in
      Ok (expr_node (If (condition, then_expr, else_expr)) location)

  let parse_match_case () = 
    let* pattern = parse_pattern () in 
    let _ = Stream.advance () in 
    let* _ = Stream.expect "->" in 
    let* expr = expr () in 
      Ok {lhs = pattern; rhs = expr}

  let parse_match_cases () = 
    let rec parse_match_cases_aux cases =
      if Stream.accept "|" then 
        let* case = parse_match_case () in
          parse_match_cases_aux (case :: cases)
      else 
          Ok (List.rev cases)
    in 
      let* case = parse_match_case () in 
      parse_match_cases_aux [case] 

  let parse_pattern_match () = 
    let match_tok = Stream.prev () in
    let* match_expr = expr () in 
    let* _ = Stream.expect "with" in
    let* cases = parse_match_cases () in 
    let* pos = Stream.pos () in
    let location = {
      line = match_tok.line; 
      col = match_tok.col; 
      start_pos = match_tok.pos; 
      end_pos = pos
    } in
      Ok (expr_node (Match(match_expr, cases)) location)

  let parse_type_con_type token = 
    match token.lexeme with 
        "int" -> Ok (App(TInt, [])) 
      | "float" -> Ok (App(TInt, []))  
      | "string" -> Ok (App(TInt, [])) 
      | "bool" -> Ok (App(TInt, [])) 
      | "unit" -> Ok (App(TUnit, [])) 
      | _ -> Error (Unsupported_type (Printf.sprintf "unsupported type %s for type constructor" (stringify_token token)))

  let parse_type_constructor type_def_name = 
    let* ident = Stream.expect "ident" in 
    let* typ = if Stream.accept "of" then
        let* annotation = Stream.expect "annotation" in 
        let* typ = parse_type_con_type annotation in 
          Ok (Some typ)
      else 
        Ok None
    in 
      Ok ({type_def_name = type_def_name; con_name = ident.lexeme; of_type = typ})

  let parse_type_constructors type_def_name = 
    let rec parse_type_constructors_aux typ_cons =
      if Stream.accept "|" then 
        let* typ_con = parse_type_constructor type_def_name in
          parse_type_constructors_aux (typ_con :: typ_cons)
      else 
          Ok (List.rev typ_cons)
    in 
      let* typ_con = parse_type_constructor type_def_name in 
        parse_type_constructors_aux [typ_con]

  let parse_type_definition () = 
    let* type_tok = Stream.expect "type" in 
    let* ident = Stream.expect "ident" in 
    let* _ = Stream.expect "=" in 
    let* variants = parse_type_constructors ident.lexeme in
    let* pos = Stream.pos () in 
    let location = {
      line = type_tok.line;
      col = type_tok.col;
      start_pos = type_tok.pos;
      end_pos = pos
    } in
      Ok (TypeDefintion ({type_name = ident.lexeme; type_constructors = variants}, location))

  let parse_module_item () =
    let* next = Stream.take () in 
    match next.token_type with 
      Keywords Type -> parse_type_definition () 
    | _ -> 
      let* expr = expr () in match expr.expr_desc with 
        Let (is_rec, value_binding, body) -> 
          (match body with 
              None -> Ok (LetDeclaration (is_rec, value_binding, expr.loc)) 
            | Some _ -> Ok (Expr expr))
        | _ -> Ok (Expr expr)

  let parse_program () = 
    let rec parse_program_aux module_items = 
      if Stream.eof () then 
        Ok (List.rev module_items)
      else 
        let* item = parse_module_item () in 
          parse_program_aux (item :: module_items)
    in 
      parse_program_aux []

  let loc_of_bin_op left right = {
    line = left.loc.line; 
    col = left.loc.col; 
    start_pos = left.loc.start_pos; 
    end_pos = right.loc.end_pos
  }

  let make_binary lhs = 
    let op = Stream.prev () in
    let op_expr = {expr_desc = Ident op.lexeme; loc = loc op} in 
    let bp = Hashtbl.find bp_table op.lexeme in
    let* rhs = parse_expr bp in 
    Ok (expr_node (Apply(op_expr, [lhs; rhs])) (loc_of_bin_op lhs rhs))

  let bin_op left = 
    let op = Stream.prev () in 
    let bp = Hashtbl.find bp_table op.lexeme in
    let* right = parse_expr bp in 
    Ok (expr_node (BinOp (left, op.lexeme, right)) (loc_of_bin_op left right))

  let _ = List.iter (fun op -> Hashtbl.add prec_table op (Led make_binary)) ["+"; "*"; "-"; "/"; "+."; "-."; "*."; "/."; "<"; "<="; ">"; ">="; "="; "<>"]
  
  let _ = 
    List.iter (fun (l, h) -> Hashtbl.add prec_table l h) 
      [("(", Nud parse_grouped); ("if", Nud parse_if_expr); 
      ("let", Nud parse_let_binding); ("match", Nud parse_pattern_match); 
      ("fun", Nud parse_function)]
end

module ParserImpl = Parser(TokenStream)

let parse tokens = 
  let _ = TokenStream.init tokens in 
  ParserImpl.parse_program ()