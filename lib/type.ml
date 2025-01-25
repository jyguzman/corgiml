type ty = 
    | Nil 
    | App of tycon * ty list 
    | Var of string 
    | Poly of string list * ty

and tycon = 
    | TInt 
    | TFloat
    | TString 
    | TBool
    | TUnit 
    | TArrow 
    | TyFun of string list * ty

let string_of_type = function
  | TInt -> "int"
  | TFloat -> "float"
  | TString -> "string"
  | TBool -> "bool"
  | TUnit -> "unit"
  | TArrow -> "arrow"
  | TyFun (_, _) -> "fun"
