open Token
open Result 
open Option

type parse_error = 
  | Unexpected_eof of string
  | Unexpected_token of string
  | Invalid_rec_let_binding of string

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
  val expect: string -> (unit, parse_error) result
  val accept: string -> bool
  val take: unit -> (token, parse_error) result
  val eof: unit -> bool
end

module TokenStream : TOKEN_STREAM = struct 
  let (tokens: token list ref) = ref []

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

  let expect lexeme = match !tokens with 
    [] -> Error (Unexpected_token "unexpected end of file")
  | x :: _ -> 
    if x.lexeme = lexeme then let _ = advance () in Ok ()
    else Error (Unexpected_token ("expected " ^ lexeme ^ " but got " ^ stringify_token x))

  let accept lexeme_or_name = match !tokens with 
    [] -> false
  | x :: _ -> 
    if x.lexeme = lexeme_or_name || x.name = lexeme_or_name 
      then let _ = advance () in true else false

  let eof () = match !tokens with 
      [] -> false 
      | x :: _ -> x.token_type = EOF
end

type handler = 
  | Nud of (unit -> (Ast.expr, parse_error) result) 
  | Led of (Ast.expr -> (Ast.expr, parse_error) result)

let (prec_table: (token_type, int * handler) Hashtbl.t) = Hashtbl.create 64

let get_lbp token = 
  let prec_info = Hashtbl.find_opt prec_table token.token_type in 
  match prec_info with 
    Some (lbp, _) -> Ok lbp
  | None -> Error (Unexpected_token ("Unexpected token getting lbp " ^ stringify_token token))

let get_literal token =
  match token.token_type with 
    Literal l -> (match l with 
        Integer i -> Ok (Ast.Integer i)
      | Decimal d -> Ok (Ast.Float d)
      | String s -> Ok (Ast.String s)
      | Ident i -> Ok (Ast.Ident i))
    | _ -> Error (Unexpected_token ("Expected literal, got " ^ stringify_token token))

let get_led token = 
  let prec = Hashtbl.find_opt prec_table token.token_type in 
  match prec with 
    None -> Error (Unexpected_token ("unexpected token getting led: " ^ stringify_token token))
  | Some (_, handler) ->
    (match handler with 
      Led led -> Ok led 
      | Nud _ -> Error (Unexpected_token ("expected an infix operator or token but got " ^ stringify_token token)))

let nud token = 
  match token.token_type with 
      Literal _ -> get_literal token
    | _ -> 
      try let _, handler = Hashtbl.find prec_table token.token_type in 
      (match handler with 
        Nud n -> n ()
        | Led _ -> Error (Unexpected_token "Expected nud for lparen"))
    with _ -> 
      Error (Unexpected_token ("expected the start of an expression (literal, identifier, prefix operator, or opening delimiter) but got " ^ stringify_token token))

module Parser (Stream : TOKEN_STREAM) = struct  

  let parse_expr rbp =
    let rec parse_expr_aux left =
      if Stream.eof () then Ok left else 
      let* next = Stream.take () in
      if Hashtbl.find_opt prec_table next.token_type = None then 
        Ok left 
      else 
        let* lbp = match next.token_type with 
          Literal _-> Ok 0
          | _ -> get_lbp next 
        in 
        if rbp >= lbp then 
          Ok left
        else
          let _ = Stream.advance () in
          let* led = get_led next in 
          let* left = led left in 
            parse_expr_aux left
    in 
      let* next = Stream.take () in 
      let _ = Stream.advance () in
      let* left = nud next in 
        parse_expr_aux left

  let parse_group () = 
    let* inner = parse_expr 0 in
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
    let* body = parse_expr 0 in
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
        parse_expr 0 
      else 
        let* body = parse_expr 0 in 
          Ok (make_fn_node (List.tl idents) body)
      in
      let* _ = Stream.expect ("in") in 
      let* expr_after_in = parse_expr 0 in 
        Ok (Ast.LetBinding (is_rec, name, expr_after_equal, expr_after_in))

  let parse_if_expr () = 
    let* then_cond = parse_expr 0 in 
    let* _ = Stream.expect ("then") in 
    let* then_expr = parse_expr 0 in 
    let* else_expr = if Stream.accept ("else") then parse_expr 0 else Ok None in 
      Ok (Ast.IfExpr {
        then_cond = then_cond; 
        then_expr = then_expr; 
        else_expr = else_expr})

  let parse_match_clause () = 
    let* pattern = Stream.take () in 
    let _ = Stream.advance () in 
    let* _ = Stream.expect ("->") in 
    let* expr = parse_expr 0 in 
      Ok ({Ast.pattern = pattern.lexeme; Ast.cmp_to = expr})

  let parse_match_clauses () = 
    let rec parse_match_clauses_aux clauses = 
      if Stream.accept ("|") then 
        let* clause = parse_match_clause () in
          parse_match_clauses_aux (clause :: clauses)
      else 
          Ok (List.rev clauses)
    in 
      parse_match_clauses_aux []

  let parse_pattern_match () = 
    let* match_expr = parse_expr 0 in 
    let* _ = Stream.expect ("with") in
    let* clauses = parse_match_clauses () in 
      Ok (Ast.PatternMatch {match_expr = match_expr; clauses = clauses})
    

  let parse_type_declaration () = Ast.None

  let add_int_handler = Led (fun left -> let* right = parse_expr 20 in Ok (Ast.BinOp (Add (left, right))))
  let sub_handler = Led (fun left -> let* right = parse_expr 20 in Ok (Ast.BinOp (Subtract (left, right))))
  let mult_handler = Led (fun left -> let* right = parse_expr 30 in Ok (Ast.BinOp (Multiply (left, right))))
  let divide_handler = Led (fun left -> let* right = parse_expr 30 in Ok (Ast.BinOp (Divide (left, right))))

  let add_to_prec_table token_type bp handler = 
    Hashtbl.add prec_table token_type (bp, handler)

  let _ = add_to_prec_table (IntArithOp Plus) 20 add_int_handler
  let _ = add_to_prec_table (IntArithOp Star) 30 mult_handler
  let _ = add_to_prec_table LParen 70 (Nud parse_group)
  let _ = add_to_prec_table (Keywords If) 0 (Nud parse_if_expr)
  let _ = add_to_prec_table (Keywords Let) 0 (Nud parse_let_binding)
  let _ = add_to_prec_table (Keywords Match) 0 (Nud parse_pattern_match)
  let _ = add_to_prec_table (Keywords Fun) 0 (Nud parse_function)

end

module ParserImpl = Parser(TokenStream)

let parse tokens = 
  let _ = TokenStream.init tokens in 
  ParserImpl.parse_expr 0