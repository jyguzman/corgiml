open Token

type parse_error = 
  | Unexpected_eof of string
  | Unexpected_token of string
  | Invalid_rec_let_binding of string
  | Unsupported_type of string

let (let*) r f = match r with 
  Ok v -> f v 
| Error e -> Error e 

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
  {Ast.col = token.col; line = token.line; start_pos = token.pos; end_pos = token.pos + len - 1}

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
  | Nud of (unit -> (Ast.expression, parse_error) result) 
  | Led of (Ast.expression -> (Ast.expression, parse_error) result) 


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
    try Ok (Hashtbl.find bp_table token.lexeme)
    with _ -> Error (Unexpected_token ("Unexpected token " ^ stringify_token token))

  let expr_node expr_desc location = 
    {Ast.expr_desc = expr_desc; loc = location}

  let nud token = 
    let loc = loc token in 
    match token.token_type with 
      Literal l -> (match l with 
          Integer i -> Ok (expr_node (Ast.Literal (Integer i)) loc)
        | Decimal d -> Ok (expr_node (Ast.Literal (Float d)) loc)
        | String s -> Ok (expr_node (Ast.Literal (String s)) loc)
        | Ident i -> Ok (expr_node (Ast.Literal (Ident i)) loc))
    | Keywords True -> Ok (expr_node (Ast.Literal (Bool true)) loc)
    | Keywords False -> Ok (expr_node (Ast.Literal (Bool false)) loc)
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
      let* lbp = if Hashtbl.find_opt bp_table curr.lexeme <> None then 
        lbp curr
      else 
        Ok 70 (* function application *)
      in if rbp >= lbp then 
        Ok left
      else
        let* left = if lbp = 70 then Ok left
          (* let* arg = parse_expr 71 in 
            Ok (expr_node (Ast.FnApp (left, arg) loc)) *)
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

  let parse_grouped () = 
    let lparen = Stream.prev () in 
    let* inner = expr () in
    let* rparen = Stream.expect(")") in
    let location = {Ast.line = lparen.line; col = lparen.col; start_pos = lparen.pos; end_pos = rparen.pos} in 
      Ok (expr_node (Ast.Grouping inner) location)

  let parse_pattern () = 
    let* next = Stream.take () in 
    let loc = loc next in 
    match next.token_type with  
      Literal Integer i -> Ok {Ast.pattern_desc = Ast.ConstInteger i; loc = loc}
    | Literal Decimal d -> Ok {Ast.pattern_desc = Ast.ConstFloat d; loc = loc}
    | Literal String s -> Ok {Ast.pattern_desc = Ast.ConstString s; loc = loc}
    | Literal Ident i -> Ok {Ast.pattern_desc = Ast.ConstIdent i; loc = loc}
    | Keywords True -> Ok {Ast.pattern_desc = Ast.True; loc = loc}
    | Keywords False -> Ok {Ast.pattern_desc = Ast.False; loc = loc}
    | Special EmptyParens -> Ok {Ast.pattern_desc = Ast.EmptyParens; loc = loc}
    | Special Wildcard -> Ok {Ast.pattern_desc = Ast.Wildcard; loc = loc}
    | _ -> Error (Unexpected_token ("expected a pattern but got " ^ (stringify_token next)))

  let parse_patterns () = 
    let rec parse_patterns_aux patterns = 
      if Stream.accept_any (["ident"; "true"; "false"; "()"; "_"]) then 
        let* pattern = parse_pattern () in
        let _ = Stream.advance () in
          parse_patterns_aux (pattern :: patterns)
      else 
        Ok (List.rev patterns)
    in 
      parse_patterns_aux [] 

  (* let make_fn_node params body loc = 
    let params = List.rev params in 
      List.fold_left (fun fn param -> expr_node (Ast.Function {param = param; expr = fn}) loc) body params *)

  let parse_function () =
    let fun_token = Stream.prev () in
    let* patterns = parse_patterns () in 
    let* _ = Stream.expect ("->") in
    let* body = expr () in
    let* pos = Stream.pos () in 
    let location = Ast.{
      line = fun_token.line; 
      col = fun_token.col; 
      start_pos = fun_token.pos; 
      end_pos = pos
    } in 
      Ok (expr_node (Ast.Function (patterns, body)) location)

  let parse_value_binding let_loc =
    let* idents = parse_patterns () in
    let num_idents = List.length idents in 
    let lhs = List.hd idents in
    let* _ = Stream.expect ("=") in 
    let* rhs = if num_idents = 1 then 
      expr () 
    else 
      let* body = expr () in
      let* curr = Stream.take () in 
      let location = Ast.{line = let_loc.line; col = let_loc.col; start_pos = let_loc.start_pos; end_pos = curr.pos} in  
        Ok (expr_node (Ast.Function (List.tl idents, body)) location)
    in 
    let* pos = Stream.pos () in
    let location = Ast.{line = let_loc.line; col = let_loc.col; start_pos = let_loc.start_pos; end_pos = pos} in  
      Ok Ast.{pat = lhs; rhs = rhs; constraints = [None]; location = location}  

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
      Ast.line = let_tok.line; 
      Ast.col = let_tok.col; 
      start_pos = let_tok.pos; 
      end_pos = curr.pos} 
    in  
      Ok (expr_node (Ast.LetBinding (is_rec, value_binding, body)) location)

  let parse_if_expr () = 
    let if_tok = Stream.prev () in
    let* condition = expr () in 
    let* _ = Stream.expect ("then") in 
    let* then_expr = expr () in 
    let* else_expr = if Stream.accept "else" then 
      let* expr = expr () in Ok (Some expr)
    else 
      Ok None in  
    let* pos = Stream.pos () in 
    let location = Ast.{
      line = if_tok.line; 
      col = if_tok.col; 
      start_pos = if_tok.pos; 
      end_pos = pos
    } in
      Ok (expr_node (Ast.IfExpr (condition, then_expr, else_expr)) location)

  let parse_match_case () = 
    let* pattern = parse_pattern () in 
    let _ = Stream.advance () in 
    let* _ = Stream.expect ("->") in 
    let* expr = expr () in 
      Ok (Ast.{lhs = pattern; rhs = expr}) 

  let parse_match_cases () = 
    let rec parse_match_cases_aux cases =
      if Stream.accept ("|") then 
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
    let* _ = Stream.expect ("with") in
    let* cases = parse_match_cases () in 
    let* pos = Stream.pos () in
    let location = Ast.{
      line = match_tok.line; 
      col = match_tok.col; 
      start_pos = match_tok.pos; 
      end_pos = pos
    } in
      Ok (expr_node (Ast.Match (match_expr, cases)) location)

  let parse_type_con_type token = 
    match token.lexeme with 
        "int" -> Ok (Ast.App(Ast.TInt, [])) 
      | "float" -> Ok (Ast.App(Ast.TInt, []))  
      | "string" -> Ok (Ast.App(Ast.TInt, [])) 
      | "bool" -> Ok (Ast.App(Ast.TInt, [])) 
      | "unit" -> Ok (Ast.App(Ast.TUnit, [])) 
      | _ -> Error (Unsupported_type (Printf.sprintf "unsupported type %s for type constructor" (stringify_token token)))

  let parse_type_constructor type_def_name = 
    let* ident = Stream.expect ("ident") in 
    let* typ = if Stream.accept ("of") then
        let* annotation = Stream.expect ("annotation") in 
        let* typ = parse_type_con_type annotation in 
          Ok (Some typ)
      else 
        Ok None
    in 
      Ok ({Ast.type_def_name = type_def_name; Ast.con_name = ident.lexeme; of_type = typ})

  let parse_type_constructors type_def_name = 
    let rec parse_type_constructors_aux typ_cons =
      if Stream.accept ("|") then 
        let* typ_con = parse_type_constructor type_def_name in
          parse_type_constructors_aux (typ_con :: typ_cons)
      else 
          Ok (List.rev typ_cons)
    in 
      let* typ_con = parse_type_constructor type_def_name in 
        parse_type_constructors_aux [typ_con]

  let parse_type_definition () = 
    let* type_tok = Stream.expect ("type") in 
    let* ident = Stream.expect ("ident") in 
    let* _ = Stream.expect ("=") in 
    let* variants = parse_type_constructors ident.lexeme in
    let* pos = Stream.pos () in 
    let location = Ast.{
      line = type_tok.line;
      col = type_tok.col;
      start_pos = type_tok.pos;
      end_pos = pos
    } in
      Ok (Ast.TypeDefintion ({type_name = ident.lexeme; type_constructors = variants}, location))

  let parse_constructor_application () =
    Ast.None

  let parse_module_item () =
    let* next = Stream.take () in 
    match next.token_type with 
      Keywords Type -> parse_type_definition () 
    | _ -> 
      let* expr = expr () in match expr.expr_desc with 
        Ast.LetBinding (is_rec, value_binding, body) -> 
          (match body with 
              None -> Ok (Ast.LetDeclaration (is_rec, value_binding, expr.Ast.loc)) 
            | Some _ -> Ok (Ast.Expr expr))
        | _ -> Ok (Ast.Expr expr)

  let parse_program () = 
    let rec parse_program_aux module_items = 
      if Stream.eof () then 
        Ok (List.rev module_items)
      else 
        let* item = parse_module_item () in 
          parse_program_aux (item :: module_items)
    in 
      parse_program_aux []


  let loc_of_bin_op left right = 
    Ast.{line = left.loc.line; 
    col = left.loc.col; 
    start_pos = left.loc.start_pos; 
    end_pos = right.loc.end_pos}

  let iadd_handler = Led (fun left -> let* right = parse_expr 20 in Ok (expr_node (Ast.BinOp (IAdd (left, right))) (loc_of_bin_op left right)))
  let imult_handler = Led (fun left -> let* right = parse_expr 30 in Ok (expr_node (Ast.BinOp (IMultiply (left, right))) (loc_of_bin_op left right)))
  let isub_handler = Led (fun left -> let* right = parse_expr 20 in Ok (expr_node (Ast.BinOp (ISubtract (left, right))) (loc_of_bin_op left right)))
  let idiv_handler = Led (fun left -> let* right = parse_expr 30 in Ok (expr_node (Ast.BinOp (IDivide (left, right))) (loc_of_bin_op left right)))

  let fadd_handler = Led (fun left -> let* right = parse_expr 20 in Ok (expr_node (Ast.BinOp (FAdd (left, right))) (loc_of_bin_op left right)))
  let fmult_handler = Led (fun left -> let* right = parse_expr 30 in Ok (expr_node (Ast.BinOp (FMultiply (left, right))) (loc_of_bin_op left right)))
  let fsub_handler = Led (fun left -> let* right = parse_expr 20 in Ok (expr_node (Ast.BinOp (FSubtract (left, right))) (loc_of_bin_op left right)))
  let fdiv_handler = Led (fun left -> let* right = parse_expr 30 in Ok (expr_node (Ast.BinOp (FDivide (left, right))) (loc_of_bin_op left right)))

  let less_handler = Led (fun left -> let* right = parse_expr 10 in Ok (expr_node (Ast.BinOp (Less (left, right))) (loc_of_bin_op left right)))
  let leq_handler = Led (fun left -> let* right = parse_expr 10 in Ok (expr_node (Ast.BinOp (Leq (left, right))) (loc_of_bin_op left right)))

  let greater_handler = Led (fun left -> let* right = parse_expr 10 in Ok (expr_node (Ast.BinOp (Greater (left, right))) (loc_of_bin_op left right)))
  let geq_handler = Led (fun left -> let* right = parse_expr 10 in Ok (expr_node (Ast.BinOp (Geq (left, right))) (loc_of_bin_op left right)))

  let eq_handler = Led (fun left -> let* right = parse_expr 10 in Ok (expr_node (Ast.BinOp (Eq (left, right))) (loc_of_bin_op left right)))
  let neq_handler = Led (fun left -> let* right = parse_expr 10 in Ok (expr_node (Ast.BinOp (Neq (left, right))) (loc_of_bin_op left right)))

  let set_handler lexeme handler = Hashtbl.add prec_table lexeme handler

  let _= List.iter2 (fun t h -> set_handler t h) ["+"; "*"; "-"; "/"] [iadd_handler; imult_handler; isub_handler; idiv_handler]
  let _= List.iter2 (fun t h -> set_handler t h) ["+."; "*."; "-."; "/."] [fadd_handler; fmult_handler; fsub_handler; fdiv_handler]
  let _= List.iter2 (fun t h -> set_handler t h) ["<"; "<="; ">"; ">="; "="; "<>"] [less_handler; leq_handler; greater_handler; geq_handler; eq_handler; neq_handler]

  let _ = set_handler "(" (Nud parse_grouped)
  let _ = set_handler "if" (Nud parse_if_expr)
  let _ = set_handler "let" (Nud parse_let_binding)
  let _ = set_handler "match" (Nud parse_pattern_match)
  let _ = set_handler "fun" (Nud parse_function)

end

module ParserImpl = Parser(TokenStream)

let parse tokens = 
  let _ = TokenStream.init tokens in 
  ParserImpl.parse_program ()