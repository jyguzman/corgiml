open Token
open Result 
open Option

type parse_error = 
  | Unexpected_eof of string
  | Unexpected_token of string
  | Invalid_rec_let_binding of string
  | Unsupported_type of string

let (let+) o f = match o with 
  Some v -> f v 
| None -> None 

let (let*) r f = match r with 
  Ok v -> f v 
| Error e -> Error e 

module type TOKEN_STREAM = sig 
  val init: token list -> unit
  val peek: unit -> token option 
  val advance: unit -> unit 
  val expect: string -> (token, parse_error) result
  val accept: string -> bool
  val take: unit -> (token, parse_error) result
  val eof: unit -> bool
end

module TokenStream : TOKEN_STREAM = struct 
  let tokens = ref []

  let init (toks: token list) = tokens := toks 

  let peek () = match !tokens with 
    [] -> None 
  | x :: _ -> Some x

  let advance () = match !tokens with 
    [] -> () 
  | _ :: xs -> tokens := xs

  let take () = match !tokens with 
    [] -> Error (Unexpected_eof "unexpected end of file")  
  | x :: _ -> Ok x

  let expect lexeme_or_name = match !tokens with 
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

  let eof () = match !tokens with 
      | [] | [_] -> true 
      | _ -> false
end

type handler = 
  | Nud of (unit -> (Ast.expr, parse_error) result) 
  | Led of (Ast.expr -> (Ast.expr, parse_error) result)


module Parser (Stream : TOKEN_STREAM) = struct  
  let prec_table = Hashtbl.create 64
  let bp_table = Hashtbl.create 64

  let set_bp op_lexeme bp = Hashtbl.add bp_table op_lexeme bp

  let _ = set_bp "if" 5
  let _ = List.iter (fun op -> set_bp op 10) ["<"; "<="; ">"; ">="; "="]
  let _ = List.iter (fun op -> set_bp op 20) ["+"; "+."; "-"; "-."]
  let _ = List.iter (fun op -> set_bp op 30) ["*"; "*."; "/"; "/."]
  let _ = List.iter (fun t -> set_bp t 0) ["fun"; "let"; "match"] 
  let _ = List.iter (fun t -> set_bp t 0) [")"; "->"; "|"; "of"; "in"; "type"; "with"; "then"; "else"; "eof"]

  let lbp token =   
    try Ok (Hashtbl.find bp_table token.lexeme)
    with _ -> Error (Unexpected_token ("Unexpected token " ^ stringify_token token))

  let nud token = 
    match token.token_type with 
      Literal l -> (match l with 
          Integer i -> Ok (Ast.Integer i)
        | Decimal d -> Ok (Ast.Float d)
        | String s -> Ok (Ast.String s)
        | Ident i -> Ok (Ast.Ident i))
    | Keywords True -> Ok (Ast.Bool true)
    | Keywords False -> Ok (Ast.Bool false)
    | _ -> 
      try match Hashtbl.find prec_table token.lexeme with 
        Nud nud -> nud ()
      | Led _ -> Error (Unexpected_token ("Unexpected token for start of expression: " ^ stringify_token token))
      with _ -> 
        Error (Unexpected_token ("expected the start of an expression (literal, identifier, prefix operator, or opening delimiter) but got " ^ stringify_token token))

  let rec led left token = 
    let prec = Hashtbl.find_opt prec_table token.lexeme in 
    match prec with 
      None -> Error (Unexpected_token ("unexpected token " ^ stringify_token token))
    | Some handler ->
      (match handler with 
          Led led -> let* expr = led left in Ok expr
        | Nud _ -> Error (Unexpected_token ("expected an infix operator or token but got " ^ stringify_token token)))

  and expr () = 
    parse_expr 0

  and parse_expr rbp =
    let rec parse_expr_aux left =
      let* curr = Stream.take () in
      let* lbp = if Hashtbl.find_opt bp_table curr.lexeme <> None then 
        lbp curr
      else 
        Ok 70 
      in if rbp >= lbp then 
        Ok left
      else
        let* left = if lbp = 70 then 
          let* arg = parse_expr 71 in 
            Ok (Ast.FnApp {fn = left; arg = arg})
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

  let parse_group () = 
    let* inner = expr () in
    let* _ = Stream.expect(")") in
      Ok (Ast.Grouping inner)  
    
  let parse_params () = 
    let rec parse_params_aux params = 
      let* ident = Stream.take () in
      match ident.token_type with 
        Literal Ident i -> 
          let _ = Stream.advance () in  
            parse_params_aux (i :: params) 
      | _ -> 
          Ok (List.rev params)
  in 
    parse_params_aux [] 
    
  let make_fn_node params body = 
    let params = List.rev params in 
      List.fold_left (fun fn param -> Ast.Function {param = param; expr = fn}) body params

  let parse_function () =
    let* params = parse_params () in 
    let* _ = Stream.expect ("->") in
    let* body = expr () in
      Ok (make_fn_node params body)

  let parse_let_binding () = 
    let is_rec = Stream.accept ("rec") in 
    let* idents = parse_params () in
    let num_idents = List.length idents in 
    if is_rec && num_idents < 2 then 
      let* curr_token = Stream.take () in 
      Error (Invalid_rec_let_binding ("'rec' expects at least two identifiers: function name then parameters at line " ^ string_of_int curr_token.line))
    else 
      let name = List.hd idents in
      let* _ = Stream.expect ("=") in 
      let* expr_after_equal = if num_idents = 1 then 
        expr () 
      else 
        let* body = expr () in 
          Ok (make_fn_node (List.tl idents) body)
      in
      let* expr_after_in = if Stream.accept ("in") then
        let* expr = expr () in Ok (Some expr)
      else 
        Ok None 
      in 
        Ok (Ast.LetBinding (is_rec, name, expr_after_equal, expr_after_in))

  let parse_if_expr () = 
    let* then_cond = expr () in 
    let* _ = Stream.expect ("then") in 
    let* then_expr = expr () in 
    let* else_expr = if Stream.accept ("else") then 
      let* expr = expr () in Ok (Some expr)
    else 
      Ok None in  
    Ok (Ast.IfExpr {
      then_cond = then_cond; 
      then_expr = then_expr; 
      else_expr = else_expr})

  let parse_pattern () = 
    let* next = Stream.take () in 
    match next.token_type with  
      Literal Integer i -> Ok (Ast.ConstInteger i)
    | Literal Decimal d -> Ok (Ast.ConstFloat d)
    | Literal String s -> Ok (Ast.ConstString s)
    | Literal Ident i -> Ok (Ast.ConstIdent i)
    | Keywords True -> Ok (Ast.True)
    | Keywords False -> Ok (Ast.False)
    | Special Brackets -> Ok (Ast.EmptyBrackets)
    | Special Wildcard -> Ok (Ast.Wildcard)
    | _ -> Error (Unexpected_token ("expected a pattern but got " ^ (stringify_token next)))

  let parse_match_clause () = 
    let* pattern = parse_pattern () in 
    let _ = Stream.advance () in 
    let* _ = Stream.expect ("->") in 
    let* expr = expr () in 
      Ok ({Ast.pattern = pattern; Ast.cmp_to = expr}) 

  let parse_match_clauses () = 
    let rec parse_match_clauses_aux clauses =
      if Stream.accept ("|") then 
        let* clause = parse_match_clause () in
          parse_match_clauses_aux (clause :: clauses)
      else 
          Ok (List.rev clauses)
    in 
      let* clause = parse_match_clause () in 
        parse_match_clauses_aux [clause] 

  let parse_pattern_match () = 
    let* match_expr = expr () in 
    let* _ = Stream.expect ("with") in
    let* clauses = parse_match_clauses () in 
      Ok (Ast.PatternMatch {match_expr = match_expr; clauses = clauses})

  let parse_type_con_type token = 
    match token.lexeme with 
        "int" -> Ok Ast.TInt 
      | "float" -> Ok Ast.TFloat 
      | "string" -> Ok Ast.TString
      | "bool" -> Ok Ast.TBool 
      | "None" -> Ok Ast.TNone 
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
    let* _ = Stream.expect ("type") in 
    let* ident = Stream.expect ("ident") in 
    let* _ = Stream.expect ("=") in 
    let* variants = parse_type_constructors ident.lexeme in
      Ok (Ast.TypeDefintion {type_name = ident.lexeme; type_constructors = variants})

  let parse_constructor_application () =
    Ast.None

  let parse_module_item () =
    let* next = Stream.take () in 
    match next.token_type with 
      Keywords Type -> parse_type_definition () 
    | _ -> let* expr = expr () in Ok (Ast.Expr expr)

  let parse_program () = 
    let rec parse_program_aux module_items = 
      if Stream.eof () then 
        Ok (List.rev module_items)
      else 
        let* item = parse_module_item () in 
          parse_program_aux (item :: module_items)
    in 
      parse_program_aux []

  let iadd_handler = Led (fun left -> let* right = parse_expr 20 in Ok (Ast.BinOp (IAdd (left, right))))
  let imult_handler = Led (fun left -> let* right = parse_expr 30 in Ok (Ast.BinOp (IMultiply (left, right))))
  let isub_handler = Led (fun left -> let* right = parse_expr 20 in Ok (Ast.BinOp (ISubtract (left, right))))
  let idiv_handler = Led (fun left -> let* right = parse_expr 30 in Ok (Ast.BinOp (IDivide (left, right))))

  let fadd_handler = Led (fun left -> let* right = parse_expr 20 in Ok (Ast.BinOp (IAdd (left, right))))
  let fmult_handler = Led (fun left -> let* right = parse_expr 30 in Ok (Ast.BinOp (IMultiply (left, right))))
  let fsub_handler = Led (fun left -> let* right = parse_expr 20 in Ok (Ast.BinOp (ISubtract (left, right))))
  let fdiv_handler = Led (fun left -> let* right = parse_expr 30 in Ok (Ast.BinOp (IDivide (left, right))))

  let less_handler = Led (fun left -> let* right = parse_expr 10 in Ok (Ast.BinOp (Less (left, right))))
  let leq_handler = Led (fun left -> let* right = parse_expr 10 in Ok (Ast.BinOp (Leq (left, right))))

  let greater_handler = Led (fun left -> let* right = parse_expr 10 in Ok (Ast.BinOp (Greater (left, right))))

  let geq_handler = Led (fun left -> let* right = parse_expr 10 in Ok (Ast.BinOp (Geq (left, right))))

  let set_handler lexeme handler = Hashtbl.add prec_table lexeme handler

  let _= List.iter2 (fun t h -> set_handler t h) ["+"; "*"; "-"; "/"] [iadd_handler; imult_handler; isub_handler; idiv_handler]
  let _= List.iter2 (fun t h -> set_handler t h) ["+."; "*."; "-."; "/."] [fadd_handler; fmult_handler; fsub_handler; fdiv_handler]
  let _= List.iter2 (fun t h -> set_handler t h) ["<"; "<="; ">"; ">="] [less_handler; leq_handler; greater_handler; geq_handler]

  let _ = set_handler "(" (Nud parse_group)
  let _ = set_handler "if" (Nud parse_if_expr)
  let _ = set_handler "let" (Nud parse_let_binding)
  let _ = set_handler "match" (Nud parse_pattern_match)
  let _ = set_handler "fun" (Nud parse_function)

end

module ParserImpl = Parser(TokenStream)

let parse tokens = 
  let _ = TokenStream.init tokens in 
  ParserImpl.parse_program ()