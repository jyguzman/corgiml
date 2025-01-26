open Token

let cut_first_n str n = 
  let len = String.length str in 
    if len <= n then ""
    else String.sub str n (len - n) 

module Keywords = Map.Make(String);;

let keywords = Keywords.of_seq @@ List.to_seq [
  ("fun", Keywords Fun); 

  ("match", Keywords Match); ("with", Keywords With);

  ("let", Keywords Let); ("rec", Keywords Rec); ("in", Keywords In);

  ("if", Keywords If); ("else", Keywords Else); ("then", Keywords Then);

  ("true", Keywords True); ("false", Keywords False);

  ("begin", Keywords Begin); ("end", Keywords End);

  ("of", Keywords Of); ("type", Keywords Type);

  ("int", Annotation TInt); ("float", Annotation TFloat); ("string", Annotation TString);
  ("bool", Annotation TBool); ("None", Annotation TNone)

];;

type lexer = {
  source: string;
  current: string;
  line: int;
  col: int;
  pos: int;
  tokens: token list;
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
    let token_type = Literal (String str) in
    let token = Token.make "string" token_type str lexer.line lexer.col in
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
        "decimal", Literal (Decimal(float_of_string num_string))
      else 
        "integer", Literal (Integer(int_of_string num_string))
    in
    let token = Token.make name token_type num_string lexer.line lexer.col in
    let new_tokens = token :: updated_lexer.tokens in
    Tokenizer.make lexer.source updated_lexer.current updated_lexer.line updated_lexer.col updated_lexer.pos new_tokens 

let tokenize_ident lexer = 
  let rec tokenize_ident lexer acc =
    if String.length lexer.current = 0 then acc, lexer
    else
      let c = String.get lexer.current 0 in
      let c_str = String.make 1 c in
      let rest = cut_first_n lexer.current 1 in
      let acc_new = acc ^ c_str in match c with 
        | 'a' .. 'z' | 'A' .. 'Z' | '_' -> tokenize_ident {lexer with current = rest; col = lexer.col + 1; pos = lexer.pos + 1} acc_new 
        | _ -> acc, lexer
  in
    let ident, updated_lexer = tokenize_ident lexer "" in
    let keyword = Keywords.find_opt ident keywords in 
    let name, token_type = match keyword with 
      | Some keyword_type -> 
        (match keyword_type with 
          Annotation _ -> "annotation", keyword_type
          | _ -> ident, keyword_type)
      | None -> "ident", Literal (Ident ident)
    in
    let token = Token.make name token_type ident lexer.line lexer.col in
    let updated_tokens = token :: updated_lexer.tokens in
    Tokenizer.make lexer.source updated_lexer.current updated_lexer.line updated_lexer.col updated_lexer.pos updated_tokens

let tokenize_op lexer c = 
  let next = peek lexer 1 in 
  let name, op, lexeme = match c with

    | '(' -> if next = ')' then ("empty_parens", Special EmptyParens, "()") else ("lparen", LParen, "(") 
    | ')' -> ("rparen", RParen, ")") 
    | ';' -> if next = ';' then ("double_semicolon", DoubleSemicolon, ";;") else ("semicolon", Semicolon, ";")
    | '[' -> if next = ']' then ("brackets", Special Brackets, "[]") else ("lbracket", LBracket, "]")
    | ':' -> if next = ':' then ("cons", Special Cons, "::") else ("colon", Colon, ":")
    | '*' -> if next = '.' then ("star", FloatArithOp StarDot, "*.") else ("star", IntArithOp Star, "*")
    | '+' -> if next = '.' then ("plus", FloatArithOp PlusDot, "+.") else ("plus", IntArithOp Plus, "+")
    | '/' -> if next = '.' then ("slash", FloatArithOp SlashDot, "/.") else ("slash", IntArithOp Slash, "/")

    | '<' -> (match next with 
      '>' -> ("left_right_arrow", ComparisonOp LeftRightArrow, "<>")
      |'=' -> ("leq", ComparisonOp Leq, "<=")
      | _ -> ("less", ComparisonOp Less, "<"))

    | '-' -> (match next with 
      '>' -> ("arrow", Special Arrow, "->")
      |'.' -> ("minus_dot", FloatArithOp MinusDot, "-.")
      | _ -> ("minus", IntArithOp Minus, "-"))
      
    | '>' -> if next = '=' then ("geq", ComparisonOp Geq, ">=") else ("greater", ComparisonOp Greater, ">")
    | '=' -> if next = '=' then ("double_equal", ComparisonOp DoubleEqual, "==") else ("equal", ComparisonOp SingleEqual, "=")
    | '!' -> if next = '=' then ("not_equal", ComparisonOp BangEqual, "!=") else Tokenizer.raise_invalid_token "!" lexer.line lexer.col

    | '|' -> if next = '|' then ("or", Logical Or, "||") else ("clause_sep", Special PttrnSeperator, "|") 
    | '&' -> if next = '&' then ("and", Logical And, "&&") else Tokenizer.raise_invalid_token "&" lexer.line lexer.col 
    | '_' -> ("wildcard", Special Wildcard, "_")
    
    | _ -> Tokenizer.raise_invalid_token (String.make 1 c) lexer.line lexer.col
  in 
  let token = Token.make name op lexeme lexer.line lexer.col in
  let n = String.length token.lexeme in 
  let skip = cut_first_n lexer.current n in 
  {lexer with col = lexer.col + n; pos = lexer.pos + n; current = skip; tokens = token :: lexer.tokens}

let tokenize_source source = 
  let rec tokenize_source lexer = 
    if String.length lexer.current = 0 then 
      let eof = Token.make "eof" EOF "eof" lexer.line lexer.col in {lexer with tokens = eof :: lexer.tokens} 
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
    List.rev processed_lexer.tokens