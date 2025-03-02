type token_type = 
  | Integer of int
  | Decimal of float
  | String of string
  | Ident of string

  | Plus
  | Slash 
  | Star
  | Minus 

  | Plus_dot
  | Slash_dot
  | Star_dot
  | Minus_dot

  | Less 
  | Less_equal
  | Greater 
  | Greater_equal
  | Single_equal
  | Double_equal
  | Bang_equal
  | Less_greater

  | Double_ampersand
  | Double_vertical_bar
  
  | Let | Rec | In | And | Fun
  | Match | With
  | If | Else | Then
  | True | False
  | Begin | End
  | Type | Of

  | L_paren 
  | R_paren
  | L_bracket 
  | R_bracket
  | L_brace 
  | R_brace
  | Semicolon
  | Double_semicolon

  | Equal_right
  | Dash_right (* -> *) 
  | Empty_brackets 
  | Wildcard 
  | Empty_parens
  | Vertical_bar
  | Colon 
  | Double_colon
  | Comma

  | Int_annotation
  | Float_annotation 
  | String_annotation
  | Bool_annotation
  | Unit_annotation

  | Eof

type token = {
  name: string;
  token_type: token_type;
  lexeme: string;
  line: int;
  col: int;
  pos: int
}

let stringify_token token = String.concat "" [
  String.uppercase_ascii token.name; "(\""; 
  token.lexeme ^ "\" line "; 
  string_of_int token.line ^ ", col " ^ string_of_int token.col; ")"
]

let rec stringify_tokens = function 
  | [] -> ""
  | [x] -> stringify_token x
  | x :: xs -> stringify_token x ^ ", " ^ stringify_tokens xs

module Token = struct 
  let make name token_type lexeme line col pos = 
    {name = name; token_type = token_type; lexeme = lexeme; line = line; col = col; pos = pos}     
end