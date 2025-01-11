type int_arith = 
  | Plus
  | Slash 
  | Star
  | Minus 

type float_arith = 
  | PlusDot
  | SlashDot
  | StarDot
  | MinusDot

type comparison = 
  | Less 
  | Leq
  | Greater 
  | Geq
  | SingleEqual
  | DoubleEqual
  | BangEqual
  | LeftRightArrow

type logical = And | Or

type keywords = 
  | Let | Rec | In
  | Match | With
  | If | Else | Then
  | True | False
  | Fun
  | Begin | End
  | Of
  | Type

type literal = 
  | Integer of int
  | Decimal of float
  | String of string
  | Ident of string

type special = 
  | Arrow | Brackets 
  | Wildcard 
  | PttrnSeperator
  | Cons


type type_annotation = 
  | TInt 
  | TFloat
  | TBool
  | TString 
  | TNone 

type token_type = 
  | Primary
  | Annotation of type_annotation
  | Literal of literal
  | IntArithOp of int_arith
  | FloatArithOp of float_arith
  | ComparisonOp of comparison
  | Minus
  | Keywords of keywords
  | Special of special
  | EOF
  | LParen 
  | RParen
  | LBracket 
  | RBracket
  | Colon 
  | Semicolon

let get_token_type token_type = 
  match token_type with 
    Literal _ -> Primary
  | _ -> token_type

type token = {
  name: string;
  token_type: token_type;
  lexeme: string;
  line: int;
  col: int
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
  let make name token_type lexeme line col = {name = name; token_type = token_type; lexeme = lexeme; line = line; col = col}
end