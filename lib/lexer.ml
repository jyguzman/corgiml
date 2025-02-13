open Token

let cut_first_n str n = 
  let len = String.length str in 
    if len <= n then ""
    else String.sub str n (len - n) 

module Keywords = Map.Make(String);;

let keywords = Keywords.of_seq @@ List.to_seq [
  ("fun", Fun); 

  ("match", Match); ("with", With);

  ("let", Let); ("rec", Rec); ("in", In);

  ("if", If); ("else", Else); ("then", Then);

  ("true", True); ("false", False);

  ("begin", Begin); ("end", End); 

  ("of", Of); ("type", Type); ("and", And);

  ("Int", Int_annotation); ("Float", Float_annotation); ("String", String_annotation);
  ("Bool", Bool_annotation); ("Unit", Unit_annotation)

];;

type lexer = {
  source: string;
  current: string;
  line: int;
  col: int;
  pos: int;
  tokens: token list; 
}

type source = {
  raw: string;
  lines: string list;
  map: (int, int) Hashtbl.t;
  tokens: token list
}

module Tokenizer = struct
  exception Invalid_token of string
  exception Unterminated_string of string

  let raise_invalid_token lexeme line col = 
    let message = "Invalid token '" ^ lexeme ^ "' at line " ^ (string_of_int line) ^ ", col " ^ (string_of_int @@ col+1) in
    let exc = Invalid_token message in raise exc

  let raise_unterminated_str lexeme line col = 
    let message = "Unterminated string '" ^ lexeme ^ "' starting at line " ^ (string_of_int line) ^ ", col " ^ (string_of_int @@ col+1) in
    let exc = Unterminated_string message in raise exc

  let create source = {source = source; current = source; line = 0; col = 0; pos = 0; tokens = []}
  let make source current line col pos tokens = {
    source = source; 
    current = current; 
    line = line; 
    col = col; 
    pos = pos; 
    tokens = tokens}
end

let peek lexer n = 
  let len = String.length lexer.current in
  if n >= len then '\x00'
  else String.get lexer.current n

let next lexer = peek lexer 1

let src_map = Hashtbl.create 128

let tokenize_string lexer quote = 
  let rec tokenize_string_aux lexer acc =
    if String.length lexer.current = 0 then Tokenizer.raise_unterminated_str acc lexer.line lexer.col
    else
      let c = String.get lexer.current 0 in
      let c_str = String.make 1 c in
      let acc_new = acc ^ c_str in  
      let rest = cut_first_n lexer.current 1 in            
      if (c = '"' && quote = '"') || (c = '\'' && quote == '\'') then 
        let lexer = {lexer with current = cut_first_n lexer.current 1} in (acc, lexer)
      else
        tokenize_string_aux {lexer with current = rest; col = lexer.col + 1; pos = lexer.pos + 1} acc_new
  in
    let str, updated_lexer = tokenize_string_aux lexer "" in
    let token_type = String str in
    let token = Token.make "string" token_type str lexer.line lexer.col lexer.pos in
    let new_tokens = token :: updated_lexer.tokens in
    Tokenizer.make lexer.source updated_lexer.current updated_lexer.line updated_lexer.col updated_lexer.pos new_tokens 
    

let tokenize_number lexer = 
  let rec tokenize_number lexer acc =
    if String.length lexer.current = 0 then acc, lexer
    else
      let c = String.get lexer.current 0 in
      let c_str = String.make 1 c in
      let acc_new = acc ^ c_str in  
      let rest = cut_first_n lexer.current 1 in 
      let updated_lexer = tokenize_number {lexer with current = rest; col = lexer.col + 1; pos = lexer.pos + 1} acc_new in   
      match c with 
        | '0' .. '9' -> updated_lexer
        | '.' -> if String.contains acc '.' then (acc, lexer) else updated_lexer
        | _ -> acc, lexer
  in
    let num_string, updated_lexer = tokenize_number lexer "" in
    let name, token_type = 
      if String.contains num_string '.' then 
        "decimal", Decimal(float_of_string num_string)
      else 
        "integer", Integer(int_of_string num_string)
    in
    let token = Token.make name token_type num_string lexer.line lexer.col lexer.pos in
    let new_tokens = token :: updated_lexer.tokens in
    let _ = Hashtbl.add src_map updated_lexer.pos updated_lexer.line in
    Tokenizer.make lexer.source updated_lexer.current updated_lexer.line updated_lexer.col updated_lexer.pos new_tokens 

let tokenize_ident lexer = 
  let rec tokenize_ident_aux lexer acc =
    if String.length lexer.current = 0 then acc, lexer
    else
      let c = String.get lexer.current 0 in
      let c_str = String.make 1 c in
      let rest = cut_first_n lexer.current 1 in
      let acc_new = acc ^ c_str in match c with 
        | 'a' .. 'z' | 'A' .. 'Z' | '_' -> tokenize_ident_aux {lexer with current = rest; col = lexer.col + 1; pos = lexer.pos + 1} acc_new 
        | _ -> acc, lexer
  in
    let ident, updated_lexer = tokenize_ident_aux lexer "" in
    let keyword = Keywords.find_opt ident keywords in 
    let name, token_type = match keyword with 
        Some keyword_type -> 
        (match keyword_type with 
          Int_annotation | Float_annotation | String_annotation
          | Bool_annotation | Unit_annotation -> "annotation", keyword_type
          | _ -> ident, keyword_type)
      | None -> "ident", Ident ident
    in
    let token = Token.make name token_type ident lexer.line lexer.col lexer.pos in
    let updated_tokens = token :: updated_lexer.tokens in
    let _ = Hashtbl.add src_map lexer.pos lexer.line in
    Tokenizer.make lexer.source updated_lexer.current updated_lexer.line updated_lexer.col updated_lexer.pos updated_tokens

let tokenize_op lexer c = 
  let next = peek lexer 1 in 
  let name, op, lexeme = match c with

    | ',' -> ("comma", Comma, ",")
    | ')' -> ("rparen", R_paren, ")") 
    | ']' -> ("rbracket", R_bracket, "]") 
    | '{' -> ("lbrace", L_brace, "{") 
    | '}' -> ("rbrace", R_brace, "}")
    | '_' -> ("wildcard", Wildcard, "_")

    | '>' -> if next = '=' then ("greater_equal", Greater_equal, ">=") else ("greater", Greater, ">")
    | '=' -> if next = '=' then ("double_equal", Double_equal, "==") else ("single_equal", Single_equal, "=")
    | '!' -> if next = '=' then ("bang_equal", Bang_equal, "!=") else Tokenizer.raise_invalid_token "!" lexer.line lexer.col

    | '|' -> if next = '|' then ("double_vertical_bar", Double_vertical_bar, "||") else ("vertical_bar", Vertical_bar, "|") 
    | '&' -> if next = '&' then ("double_ampersand", Double_ampersand, "&&") else Tokenizer.raise_invalid_token "&" lexer.line lexer.col 

    | '(' -> if next = ')' then ("empty_parens", Empty_parens, "()") else ("lparen", L_paren, "(") 
    | ';' -> if next = ';' then ("double_semicolon", Double_semicolon, ";;") else ("semicolon", Semicolon, ";")
    | '[' -> if next = ']' then ("brackets", Empty_brackets, "[]") else ("lbracket", L_bracket, "[")
    | ':' -> if next = ':' then ("double_colon", Double_colon, "::") else ("colon", Colon, ":")
    | '*' -> if next = '.' then ("star", Star_dot, "*.") else ("star", Star, "*")
    | '+' -> if next = '.' then ("plus", Plus_dot, "+.") else ("plus", Plus, "+")
    | '/' -> if next = '.' then ("slash", Slash_dot, "/.") else ("slash", Slash, "/")

    | '<' -> (match next with 
      '>' -> ("less_greater", Less_greater, "<>")
      |'=' -> ("less_equal", Less_equal, "<=")
      | _ -> ("less", Less, "<"))

    | '-' -> (match next with 
      '>' -> ("dash_right", Dash_right, "->")
      |'.' -> ("minus_dot", Minus_dot, "-.")
      | _ -> ("minus", Minus, "-"))
    
    | _ -> Tokenizer.raise_invalid_token (String.make 1 c) lexer.line lexer.col
  in 
  let token = Token.make name op lexeme lexer.line lexer.col lexer.pos in
  let n = String.length token.lexeme in 
  let skip = cut_first_n lexer.current n in 
  let _ = Hashtbl.add src_map lexer.pos lexer.line in
  {lexer with col = lexer.col + n; pos = lexer.pos + n; current = skip; tokens = token :: lexer.tokens}

 let tokenize_source source = 
  let rec tokenize_source lexer = 
    if String.length lexer.current = 0 then 
      let eof = Token.make "eof" Eof "eof" lexer.line lexer.col lexer.pos in {lexer with tokens = eof :: lexer.tokens} 
    else
      let c = peek lexer 0 in
      let skip = cut_first_n lexer.current 1 in 
      tokenize_source @@ match c with 
        | '0' .. '9' -> tokenize_number lexer 

        | 'a' .. 'z' | 'A' .. 'Z' -> tokenize_ident lexer

        | '"' | '\'' -> tokenize_string {lexer with current = skip} c

        | '^' | '<' | '>' | '=' | '~' | '+' | '-' | '/' | '*' |'.' | '_'
        | ';' | ':' | ',' | '[' | ']' | '{' | '}' | '(' | ')' | '|'| '&' -> tokenize_op lexer c

        | '\n' -> 
          {lexer with line = lexer.line + 1; col = 0; pos = lexer.pos + 1; current = skip}

        | ' ' -> {lexer with col = lexer.col + 1; pos = lexer.pos + 1; current = skip}
        
        | _ -> Tokenizer.raise_invalid_token (String.make 1 c) lexer.line lexer.col
  in 
    let new_lexer = Tokenizer.create source in
    let processed_lexer = tokenize_source new_lexer in
    let src_lines = String.split_on_char '\n' source in 
    {raw = source; lines = src_lines; map = src_map; tokens = List.rev processed_lexer.tokens}