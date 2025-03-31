open Token
open Ast

(* TODO: 
 - Parse tuple patterns
 - Parse unary expressions
 - Parse type declarations (aliases, records, sum types)
*)

type parse_error = 
  | Unexpected_eof of string
  | Unexpected_token of string
  | Invalid_rec_let_binding of string
  | Unsupported_type of string

let (let*) r f = match r with 
  Ok v -> f v 
| Error e -> Error e 

module type TOKEN_STREAM = sig 
  val init: string -> unit
  val peek: unit -> token option 
  val pos: unit -> (int, parse_error) result
  val advance: unit -> unit 
  val prev: unit -> token
  val expect: string -> (token, parse_error) result
  val accept: string -> bool
  val accept_no_adv: string -> bool
  val accept_any: string list -> bool
  val take: unit -> (token, parse_error) result
  val next: unit -> (token, parse_error) result
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

  let take () = match !tokens with 
    [] -> Error (Unexpected_eof "unexpected end of file")  
  | x :: _ -> Ok x

  let next () = 
    let _ = advance () in 
    take ()

  let prev () = !prev

  let pos () = match !tokens with 
    [] -> Error (Unexpected_eof "unexpected end of file")  
  | x :: _ -> Ok x.pos

  let accept lexeme_or_name = match !tokens with 
    [] -> false
  | x :: _ -> 
    if x.lexeme = lexeme_or_name || x.name = lexeme_or_name then 
      let _ = advance () in true 
    else 
      false

  let expect (lexeme_or_name: string) = match !tokens with 
    [] -> Error (Unexpected_token "unexpected end of file")
  | x :: _ -> 
    if accept lexeme_or_name then 
      Ok x
    else
      let tok_str = stringify_token x in 
      Error (Unexpected_token (Printf.sprintf "Parse Error: Expected \"%s\" but received %s" lexeme_or_name tok_str))

  let accept_no_adv lexeme_or_name = match !tokens with 
    [] -> false
  | x :: _ -> 
    x.lexeme = lexeme_or_name || x.name = lexeme_or_name

  let accept_any strings = match !tokens with 
    [] -> false
  | x :: _ -> 
    List.mem x.lexeme strings || List.mem x.name strings 

  let eof () = match !tokens with 
    | [] | [_] -> true 
    | _ -> false
end

type handler = 
  | Nud of (unit -> (expression, parse_error) result) 
  | Led of (expression -> (expression, parse_error) result) 

module Parser (Stream : TOKEN_STREAM) = struct  
  let prec_table = Hashtbl.create 32
  let bp_table = Hashtbl.create 32

  let set_bp op_lexeme bp = Hashtbl.add bp_table op_lexeme bp

  let _ = set_bp "if" 5
  let _ = List.iter (fun op -> set_bp op 10) ["<"; "<="; ">"; ">="; "="]
  let _ = List.iter (fun op -> set_bp op 20) ["+"; "+."; "-"; "-."]
  let _ = List.iter (fun op -> set_bp op 30) ["*"; "*."; "/"; "/."]
  let _ = set_bp "." 40 
  let _ = List.iter (fun t -> set_bp t 0) ["fun"; "let"; "match"] 
  let _ = List.iter 
    (fun t -> set_bp t 0) 
    [","; ")"; "]"; "}"; "->"; "|"; ":"; "of"; "in"; "and";
    "type"; "with"; "then"; "else"; ";"; "eof"]

  let lbp token =   
    match Hashtbl.find_opt bp_table token.lexeme with 
      Some bp -> bp
    | None -> 70 (* assume function application if we can't find a BP *)  

  let expr_node expr_desc location = 
    {expr_desc = expr_desc; loc = location; ty = None}

  let nud token = 
    let loc = loc token in 
    match token.token_type with 
      Integer i -> Ok (expr_node (Integer i) loc)
    | Decimal d -> Ok (expr_node (Float d) loc)
    | String s -> 
      let bytes = String.to_bytes s in 
        Ok (expr_node (String (bytes, Bytes.length bytes)) loc) 
    | Ident i -> Ok (expr_node (Ident i) loc)
    | Upper_ident i -> Ok (expr_node (UpperIdent i) loc) 
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
        let* arg = parse_expr 71 in 
          parse_fn_app_aux (arg :: args)
    in 
    let* args = parse_fn_app_aux [] in 
    let span = expr_span left (List.hd args) in
    Ok (expr_node (Apply (left, List.rev args)) span)


  let ident () = 
    Stream.expect "ident"
    
  let upper_ident () = 
    Stream.expect "upper_ident"
  
  let parse_grouped () = 
    let opening = Stream.prev () in 
    let* inner = expr () in
    let* closing = if opening.token_type = Begin then 
      Stream.expect "end" 
    else 
      Stream.expect ")" 
    in
      Ok (expr_node (Grouping inner) (token_span opening closing))

  let parse_multiple parse_func separator end_token = 
    let rec parse_multiple_aux items = 
      let* curr = Stream.take() in 
      if curr.token_type = end_token then 
        Ok items 
      else
        if curr.token_type = separator then 
          let _ = Stream.advance () in
          let* curr = Stream.take() in 
          if curr.token_type = end_token then
            Ok items 
          else 
            let* item = parse_func () in 
            parse_multiple_aux (item :: items)
      else 
        Ok items
    in 
      parse_multiple_aux []

  let parse_multiple_exprs () = 
    parse_multiple expr Comma R_paren

  (* Parse a grouped expression or a tuple *)
  let parse_paren_expr () = 
    let opening = Stream.prev () in 
    let* expr = expr () in 
    let _ = print_endline (stringify_expr expr) in 
    let* curr = Stream.take () in 
    if curr.token_type = R_paren then 
      let _ = Stream.advance () in
      Ok (expr_node (Grouping expr) (token_span opening curr))
    else
      let* exprs = parse_multiple_exprs () in 
      let* closing = Stream.take () in
      let _ = Stream.advance () in
      Ok (expr_node (Tuple (expr :: List.rev exprs)) (token_span opening closing))

  let parse_pattern () = 
    let* next = Stream.take () in 
    let loc = loc next in 
    match next.token_type with  
      Integer i -> Ok {pattern_desc = Const_integer i; loc = loc}
    | Decimal d -> Ok {pattern_desc = Const_float d; loc = loc}
    | String s -> Ok {pattern_desc = Const_string s; loc = loc}
    | Ident i -> Ok {pattern_desc = Const_ident i; loc = loc}
    | True -> Ok {pattern_desc = True; loc = loc}
    | False -> Ok {pattern_desc = False; loc = loc}
    | Empty_brackets -> Ok {pattern_desc = Empty_brackets; loc = loc}
    | Empty_parens -> Ok {pattern_desc = Empty_parens; loc = loc}
    | Wildcard -> Ok {pattern_desc = Any; loc = loc}
    | _ -> Error (Unexpected_token ("expected a pattern but got " ^ (stringify_token next)))

  let parse_patterns () = 
    let rec parse_patterns_aux patterns = 
      if Stream.accept_any ["integer"; "decimal"; "string"; "ident"; 
        "true"; "false"; "()"; "_"; "[]"] then 
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

let parse_params patterns = 
  let rec parse_idents_aux patterns idents =
    if List.length patterns = 0 
      then Ok idents 
    else 
      let* ident = match (List.hd patterns).pattern_desc with 
        Const_ident i -> Ok i
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
      Ok {pat = lhs; rhs = rhs; value_constraint = None; location = location} 
      
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

  let parse_let_expr () = 
    let prev_tok = Stream.prev () in
    let is_rec = Stream.accept "rec" in 
    let* value_bindings = parse_value_bindings () in
    let* _ = Stream.expect "in" in
    let* body = expr () in
    let* curr = Stream.take () in  
    let span = token_span prev_tok curr in 
      Ok (expr_node (Let (is_rec, value_bindings, body)) span)

  let parse_if_expr () = 
    let if_tok = Stream.prev () in
    let* condition = expr () in 
    let* _ = Stream.expect "then" in
    let* then_expr = expr () in
    let* _ = Stream.expect "else" in 
    let* else_expr = expr () in 
    let* curr = Stream.take () in  
    let location = token_span if_tok curr in
      Ok (expr_node (If (condition, then_expr, else_expr)) location)

  let list () = 
    let lbracket = Stream.prev () in 
    let rec list_aux lst = 
      let* curr = Stream.take () in 
      if curr.token_type = R_bracket then 
        Ok Nil
      (* else if (Stream.prev()).token_type = Comma then 
        Error (Unexpected_token "ending of a list requires closing bracket") *)
      else
        let* head = expr () in 
        let* tail = if Stream.accept "," then  
          list_aux lst
        else 
          let* curr = Stream.take () in 
          if curr.token_type != R_bracket then 
            Error (Unexpected_token "ending of a list requires closing bracket")
          else
            Ok lst
        in
          Ok (Cons (head, tail))
    in
    let* lst = list_aux Nil in
    let* rbracket = Stream.expect "]" in 
      Ok (expr_node (List lst) (token_span lbracket rbracket))

  let record_field () =
    let* key = Stream.expect "ident" in 
    let _ = Stream.expect "=" in 
    let* value = expr () in 
      Ok {key = key.lexeme; value = value} 

  let record () = 
    let lbrace = Stream.prev () in 
    let rec record_aux fields = 
      let* curr = Stream.take () in 
      if curr.token_type = R_brace then 
        Ok fields 
      else
        let* field = record_field () in 
        let fields = field :: fields in 
        if Stream.accept "," then  
          record_aux fields
        else 
          Ok fields
    in
    let* fields = record_aux [] in 
    let* rbrace = Stream.expect "}" in 
      Ok (expr_node (Record (List.rev fields)) (token_span lbrace rbrace))

  let match_case () = 
    let* pattern = parse_pattern () in 
    let _ = Stream.advance () in 
    let* _ = Stream.expect "->" in 
    let* expr = expr () in 
      Ok {lhs = pattern; rhs = expr}

  let match_cases () = 
    let rec match_cases_aux cases =
      if Stream.accept "|" then 
        let* case = match_case () in
          match_cases_aux (case :: cases)
      else 
          Ok (List.rev cases)
    in 
      let* case = match_case () in 
        match_cases_aux [case] 

  let match_expr () = 
    let match_tok = Stream.prev () in
    let* match_expr = expr () in 
    let* _ = Stream.expect "with" in
    let* cases = match_cases () in 
    let* curr = Stream.take () in
    let span = token_span match_tok curr in
      Ok (expr_node (Match(match_expr, cases)) span)

  let rec arrow left = 
    let _ = Stream.advance () in
    let* right = ty () in
      Ok (Arrow (left, right))

  and parse_multiple_types () = 
    let rec parse_multiple_types_aux types =
      if Stream.accept_no_adv ")" then 
        Ok types 
      else
        if Stream.accept "," then
          let* typ = ty () in 
          let _ = Stream.advance () in
          parse_multiple_types_aux (typ :: types)
        else 
          Ok types
    in 
      let* first = ty () in 
      parse_multiple_types_aux [first]

  and paren_type () = 
    let* typ = ty () in 
    let* curr = Stream.next () in
    if curr.lexeme = "," then 
      let* inner = parse_multiple_types () in 
      Ok (App("Tuple", typ :: List.rev inner))
    else 
      Ok typ

  and ty () = 
    let* curr = Stream.take () in   
    let* typ = match curr.token_type with 
        Int_annotation -> Ok int 
      | Float_annotation -> Ok float
      | String_annotation -> Ok string
      | Bool_annotation -> Ok bool 
      | Unit_annotation -> Ok unit 
      | Option_annotation | Result_annotation -> 
        let* inner = ty () in Ok (App(curr.lexeme, [inner]))

      | Ident i -> Ok (Var i)

      | L_bracket -> 
        let _ = Stream.advance () in
        let* inner = ty () in 
        let* _ = Stream.expect "]" in 
        Ok (App("List", [inner]))

      | L_paren -> 
        let _ = Stream.advance () in
        let* typ = paren_type () in 
        let* _ = Stream.expect ")" in 
        Ok typ

      | _ -> 
        Error (Unsupported_type (Printf.sprintf "unsupported type %s for type constructor" (stringify_token curr)))
    in if Stream.accept "->" then 
      arrow typ
    else
      let _ = Stream.advance () in
      Ok typ

  let parse_let () = 
    let* let_tok = Stream.expect "let" in
    let is_rec = Stream.accept "rec" in 
    let* value_bindings = parse_value_bindings () in
    if Stream.accept "in" then
      let* body = expr () in
      let* curr = Stream.take () in 
      let span = token_span let_tok curr in
      let let_expr = expr_node (Let (is_rec, value_bindings, body)) span in 
        Ok {module_item_desc = Expr let_expr;
            module_item_loc = span} 
    else 
      let* curr = Stream.take () in 
      let last = if curr.token_type = Semicolon then 
        Stream.prev() 
      else 
        curr 
      in 
        Ok {module_item_desc = LetDeclaration (is_rec, value_bindings);
          module_item_loc = token_span let_tok last}

  let get_type_variables () = 
    let rec get_type_variables_aux vars = 
      if Stream.accept_no_adv "ident" then 
        let* var = ident () in 
        if String.length var.lexeme > 1 then 
          Error (Unexpected_token (Printf.sprintf "Type variables must be 1 character, but '%s' is %d characters." var.lexeme (String.length var.lexeme)))
        else
          get_type_variables_aux (var.lexeme :: vars)
      else 
        Ok vars
    in 
      let* vars = get_type_variables_aux [] in 
      Ok (List.rev vars)

  let parse_field_decl () = 
    let* key = Stream.expect "ident" in 
    let* _ = Stream.expect ":" in 
    let* ty = ty () in 
    Ok {key = key.lexeme; typ = ty}

  let parse_record_type () = 
    let rec parse_field_decls field_decls = 
      if Stream.accept "}" then 
        let _ = print_endline "yes" in
        Ok field_decls
      else
        let* field_decl = parse_field_decl () in 
        let field_decls = field_decl :: field_decls in
        if Stream.accept "," then
          parse_field_decls field_decls
        else 
          Ok field_decls
    in
    let* _ = Stream.expect "{" in 
    let* field_decls = parse_field_decls [] in
    Ok (Record (List.rev field_decls))

  let parse_variant_type () = 
    let rec parse_constr_decls decls =
      if Stream.accept "|" then 
        let* name = Stream.expect "upper_ident" in 
        let* types = ty () in 
        parse_constr_decls ({name = name.lexeme; types = [types]} :: decls)
      else
        Ok decls
    in
    let* first_name = Stream.expect "upper_ident" in 
    let* types = ty () in 
    let* rest = parse_constr_decls [{name = first_name.lexeme; types = [types]}] in
    Ok (Variant (List.rev rest)) 

  let parse_type_definition () = 
    let* type_tok = Stream.expect "type" in 
    let* type_name = Stream.expect "upper_ident" in 
    let* type_vars = get_type_variables () in 
    let* _ = Stream.expect "=" in 
    let* curr = Stream.take () in 
    let* type_kind = match curr.token_type with 
      L_brace -> parse_record_type ()
    | Vertical_bar -> let _ = Stream.advance () in parse_variant_type () 
    | Upper_ident _ -> parse_variant_type ()
    | _ -> Error (Unexpected_token ("Expected an opening brace ({) for a record type or capitalized identifier for
                                     variant type, but got " ^ stringify_token curr))
    in 
    let prev = Stream.prev () in  
    let end_pos = prev.pos + (String.length prev.lexeme - 1) in 
    Ok {module_item_desc = TypeDefinition {name = type_name.lexeme; type_vars = type_vars; type_kind = type_kind}; 
        module_item_loc = {line = type_tok.line; col = type_tok.col; start_pos = type_tok.pos; end_pos = end_pos}} 
  

  let parse_module_item () =
    let* next = Stream.take () in 
    match next.token_type with 
      Let -> parse_let ()
    | Type -> parse_type_definition () 
    | _ -> 
      let* expr = expr () in
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

  let bin_op left = 
    let op = Stream.prev () in 
    let bp = Hashtbl.find bp_table op.lexeme in
    let* right = parse_expr bp in  
    Ok (expr_node (Binary (left, (op.lexeme, loc op), right)) (expr_span left right))

  let parse_field_access left = 
    let _op = Stream.prev () in 
    let* field = Stream.expect "ident" in 
    let loc = {
      line = left.loc.line;
      col = left.loc.col;
      start_pos = left.loc.start_pos;
      end_pos = field.pos + (String.length field.lexeme) - 1
    } in
    Ok (expr_node (Field_access (left, field.lexeme)) loc)
    
  let _ = List.iter 
        (fun op -> Hashtbl.add prec_table op (Led bin_op)) 
        ["+"; "*"; "-"; "/"; "+."; "-."; "*."; "/."; "<"; "<="; ">"; ">="; "="; "<>"]

  let _ = Hashtbl.add prec_table "." (Led parse_field_access)
  
  let _ = List.iter (fun (l, h) -> Hashtbl.add prec_table l h) 
      [("(", Nud parse_paren_expr); ("begin", Nud parse_grouped); 
      ("[", Nud list); ("{", Nud record);
      ("if", Nud parse_if_expr); ("let", Nud parse_let_expr); ("match", Nud match_expr); 
      ("fn", Nud parse_function);]
end
 
module ParserImpl = Parser(TokenStream)

let parse source = 
  let _ = TokenStream.init source in 
  ParserImpl.parse_program ()

let parse_expression source = 
  let _ = TokenStream.init source in 
  ParserImpl.expr ()

let parse_type source = 
  let _ = TokenStream.init source in 
  ParserImpl.ty ()