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
  val init: string -> unit
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

let token_span (tok_one: token) tok_two =
  {line = tok_one.line; col = tok_one.col; start_pos = tok_one.pos; end_pos = tok_two.pos}

let expr_span expr_one expr_two =
  {line = expr_one.loc.line; col = expr_one.loc.col; 
  start_pos = expr_one.loc.start_pos; end_pos = expr_two.loc.end_pos}

module TokenStream : TOKEN_STREAM = struct 
  let tokens = ref []
  let prev = ref (Token.make "" Eof "" (-1) (-1) (-1))
  
  let init source = 
    tokens := (Lexer.tokenize_source source).tokens 

  let peek () = match !tokens with 
    [] -> None 
  | x :: _ -> Some x

  let advance () = match !tokens with 
    [] -> () 
  | x :: xs ->
    let _ = tokens := xs in 
      prev := x

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
  let _ = List.iter 
    (fun t -> set_bp t 0) 
    [","; ")"; "]"; "}"; "->"; "|"; ":"; "of"; "in"; "and";
    "type"; "with"; "then"; "else"; ";;"; "eof"]

  let lbp token =   
    match Hashtbl.find_opt bp_table token.lexeme with 
      Some bp -> bp
    | None -> 70 (* assume function application if we can't find a BP *)  

  let expr_node expr_desc location = 
    {expr_desc = expr_desc; loc = location}

  let nud token = 
    let loc = loc token in 
    match token.token_type with 
      Integer i -> Ok (expr_node (Integer i) loc)
    | Decimal d -> Ok (expr_node (Float d) loc)
    | String s -> 
      let bytes = String.to_bytes s in 
        Ok (expr_node (String (bytes, Bytes.length bytes)) loc) 
    | Ident i -> Ok (expr_node (Ident i) loc)
    | True -> Ok (expr_node (Bool true) loc)
    | False -> Ok (expr_node (Bool false) loc)
    | _ ->  
      try match Hashtbl.find prec_table token.lexeme with 
        Nud nud -> nud ()
      | Led _ -> Error (Unexpected_token ("expected the start of an expression (literal, identifier, prefix operator, or opening delimiter) but got " ^ stringify_token token))
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
      if lbp curr <> 70 then Ok args else
        let* arg = expr () in 
          parse_fn_app_aux (arg :: args)
    in 
    let* args = parse_fn_app_aux [] in 
    let span = expr_span left (List.hd args) in
    Ok (expr_node (Apply (left, List.rev args)) span)

  let upper_ident () = 
    let* curr = Stream.expect ("ident") in 
    let first_letter = curr.lexeme.[0] in 
    match first_letter with 
      'A'..'Z' -> Ok curr
      | _ -> Error (Unexpected_token "Expected an identifier starting with an upper case letter") 
  
  let parse_grouped () = 
    let opening = Stream.prev () in 
    let* inner = expr () in
    let* closing = if opening.token_type = Begin then 
      Stream.expect "end" 
    else 
      Stream.expect ")" 
    in
      Ok (expr_node (Grouping inner) (token_span opening closing))

  let parse_pattern () = 
    let* next = Stream.take () in 
    let loc = loc next in 
    match next.token_type with  
      Integer i -> Ok {pattern_desc = ConstInteger i; loc = loc}
    | Decimal d -> Ok {pattern_desc = ConstFloat d; loc = loc}
    | String s -> Ok {pattern_desc = ConstString s; loc = loc}
    | Ident i -> Ok {pattern_desc = ConstIdent i; loc = loc}
    | True -> Ok {pattern_desc = True; loc = loc}
    | False -> Ok {pattern_desc = False; loc = loc}
    | Empty_parens -> Ok {pattern_desc = EmptyParens; loc = loc}
    | Wildcard -> Ok {pattern_desc = Wildcard; loc = loc}
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

  (* let make_fn_node parrw2qiams body loc = 
    let params = List.rev params in 
      List.fold_left (fun fn param -> expr_node (Function {param = param; expr = fn}) loc) body params *)

let parse_params patterns = 
  let rec parse_idents_aux patterns idents =
    if List.length patterns = 0 
      then Ok idents 
    else 
      let* ident = match (List.hd patterns).pattern_desc with 
        ConstIdent i -> Ok i
      | _ -> 
        Error (Unexpected_token ("function param must be an identifier"))
      in             
        parse_idents_aux (List.tl patterns) (ident :: idents)
  in 
    parse_idents_aux patterns []

  let parse_function () =
    let fun_token = Stream.prev () in
    let* patterns = parse_patterns () in
    let* idents = parse_params patterns in 
    let* _ = Stream.expect "->" in
    let* body = expr () in
    let* curr = Stream.take () in 
    let location = token_span fun_token curr in 
      Ok (expr_node (Function (List.rev idents, body, None)) location)

  let parse_value_binding () =
    let* patterns = parse_patterns () in
    let num_idents = List.length patterns in 
    let lhs = List.hd patterns in
    let* _ = Stream.expect "=" in 
    let* rhs = if num_idents = 1 then 
      expr () 
    else 
      let* fn_body = expr () in
      let* pos = Stream.pos () in 
      let* params = parse_params (List.tl patterns) in 
      let location = {line = lhs.loc.line; col = lhs.loc.col; start_pos = lhs.loc.start_pos; end_pos = pos} in  
        Ok (expr_node (Function (List.rev params, fn_body, None)) location)
    in 
    let* pos = Stream.pos () in
    let location = {line = lhs.loc.line; col = lhs.loc.col; start_pos = lhs.loc.start_pos; end_pos = pos} in  
      Ok {pat = lhs; rhs = rhs; val_constraint = None; location = location} 
      
  let parse_value_bindings () = 
    let rec parse_value_bindings_aux bindings =
      if Stream.accept "and" then 
        let* binding = parse_value_binding () in 
          parse_value_bindings_aux (binding :: bindings)
      else
        Ok bindings
    in 
      let* binding = parse_value_binding () in 
        parse_value_bindings_aux [binding]

  let parse_let () = 
    let prev_tok = Stream.prev () in
    let is_rec = Stream.accept "rec" in 
    let* value_bindings = parse_value_bindings () in
    let* body = if Stream.accept "in" then
      let* expr = expr () in Ok (Some expr)
    else 
      Ok None 
    in 
    let* curr = Stream.take () in 
    let span = token_span prev_tok curr in
      Ok (expr_node (Let (is_rec, value_bindings, body)) span)

  let parse_if_expr () = 
    let if_tok = Stream.prev () in
    let* condition = expr () in 
    let* _ = Stream.expect "then" in 
    let* then_expr = expr () in 
    let* else_expr = if Stream.accept "else" then 
      let* expr = expr () in Ok (Some expr)
    else 
      Ok None in  
    let* curr = Stream.take () in 
    let location = token_span if_tok curr in
      Ok (expr_node (If (condition, then_expr, else_expr)) location)

  let parse_list () = 
    let lbracket = Stream.prev () in 
    let rec parse_list_aux lst = 
      let* curr = Stream.take () in 
      if curr.token_type = R_bracket then 
        Ok Nil
      (* else if (Stream.prev()).token_type = Comma then 
        Error (Unexpected_token "ending of a list requires closing bracket") *)
      else
        let* head = expr () in 
        let* tail = if Stream.accept "," then  
          parse_list_aux lst
        else 
          let* curr = Stream.take () in 
          if curr.token_type != R_bracket then 
            Error (Unexpected_token "ending of a list requires closing bracket")
          else
            Ok lst
        in
          Ok (Cons (head, tail))
    in
    let* lst = parse_list_aux Nil in
    let* rbracket = Stream.expect "]" in 
      Ok (expr_node (List lst) (token_span lbracket rbracket))

  let parse_tuple_expr () = 
    let lparen = Stream.prev () in 
    let rec parse_tuple_expr_aux tuple = 
      let* curr = Stream.take () in 
      if curr.token_type = R_paren then 
        Ok tuple 
      else
        let* expr = expr () in 
        let tuple = expr :: tuple in 
        if Stream.accept "," then  
          parse_tuple_expr_aux tuple
        else 
          Ok tuple
    in
    let* elements = parse_tuple_expr_aux [] in
    let* rparen = Stream.expect ")" in 
      Ok (expr_node (Tuple (List.rev elements)) (token_span lparen rparen))

  let parse_record_expr_field () =
    let* key = Stream.expect "ident" in 
    let _ = Stream.expect "=" in 
    let* value = expr () in 
      Ok {key = key.lexeme; value = value}

  let parse_record_expr () = 
    let lbrace = Stream.prev () in 
    let rec parse_record_expr_aux fields = 
      let* curr = Stream.take () in 
      if curr.token_type = R_brace then 
        Ok fields 
      else
        let* field = parse_record_expr_field () in 
        let fields = field :: fields in 
        if Stream.accept "," then  
          parse_record_expr_aux fields
        else 
          Ok fields
    in
    let* fields = parse_record_expr_aux [] in 
    let* rbrace = Stream.expect "}" in 
    Ok (expr_node (Record (List.rev fields)) (token_span lbrace rbrace))

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

  let parse_match () = 
    let match_tok = Stream.prev () in
    let* match_expr = expr () in 
    let* _ = Stream.expect "with" in
    let* cases = parse_match_cases () in 
    let* curr = Stream.take () in
    let span = token_span match_tok curr in
      Ok (expr_node (Match(match_expr, cases)) span)

  let parse_type_con_type token = 
    match token.lexeme with 
        "Int" -> Ok (App(TInt, [])) 
      | "Float" -> Ok (App(TInt, []))  
      | "String" -> Ok (App(TInt, [])) 
      | "Bool" -> Ok (App(TInt, [])) 
      | "Unit" -> Ok (App(TUnit, [])) 
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
    let* ident = upper_ident () in 
    let* _ = Stream.expect "=" in 
    (* Need to check what type it might be: base type, record ("{"), ADT (another ident), etc.*)
    let* variants = parse_type_constructors ident.lexeme in
    let* curr = Stream.take () in 
    let span = token_span type_tok curr in
      Ok {
        module_item_desc = TypeDefintion {type_name = ident.lexeme; type_constructors = variants}; 
        module_item_loc = span
      }

  let parse_module_item () =
    let* next = Stream.take () in 
    match next.token_type with 
      Type -> parse_type_definition ()
    | _ -> 
      let* expr = expr () in 
      match expr.expr_desc with  
        Let (is_rec, value_bindings, body) ->  
          (match body with  
              None -> Ok { 
                module_item_desc = LetDeclaration (is_rec, value_bindings);
                module_item_loc = expr.loc
              } 
            | Some _ -> 
              Ok {module_item_desc = Expr expr; module_item_loc = expr.loc})
        | _ -> 
          Ok {module_item_desc = Expr expr; module_item_loc = expr.loc}

  let parse_program () = 
    let rec parse_program_aux module_items = 
      if Stream.eof () then 
        Ok (List.rev module_items)
      else 
        let* item = parse_module_item () in 
          parse_program_aux (item :: module_items)
    in 
      parse_program_aux []

  let make_binary lhs =  
    let op = Stream.prev () in
    let op_expr = {expr_desc = Ident op.lexeme; loc = loc op} in 
    let bp = Hashtbl.find bp_table op.lexeme in
    let* rhs = parse_expr bp in 
    Ok (expr_node (Apply(op_expr, [lhs; rhs])) (expr_span lhs rhs))

  let bin_op left = 
    let op = Stream.prev () in 
    let bp = Hashtbl.find bp_table op.lexeme in
    let* right = parse_expr bp in 
    Ok (expr_node (Binary (left, op.lexeme, right)) (expr_span left right))

  let _ = List.iter 
        (fun op -> Hashtbl.add prec_table op (Led bin_op)) 
        ["+"; "*"; "-"; "/"; "+."; "-."; "*."; "/."; "<"; "<="; ">"; ">="; "="; "<>"]
  
  let _ = 
    List.iter (fun (l, h) -> Hashtbl.add prec_table l h) 
      [("(", Nud parse_grouped); ("begin", Nud parse_grouped); 
      ("[", Nud parse_list); ("(", Nud parse_tuple_expr); ("{", Nud parse_record_expr);
      ("if", Nud parse_if_expr); ("let", Nud parse_let); ("match", Nud parse_match); 
      ("fun", Nud parse_function);]
end
 
module ParserImpl = Parser(TokenStream)

let parse source = 
  let _ = TokenStream.init source in 
  ParserImpl.parse_program ()