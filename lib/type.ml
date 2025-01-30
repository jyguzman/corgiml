type ty = 
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

type ty_constraint = 
    | TyEq of string * ty
    | VarEq of string * string

let int = App(TInt, [])
let float = App(TFloat, [])
let bool = App(TBool, [])
let string = App(TString, [])
let unit = App(TUnit, [])

let string_of_tycon = function
  | TInt -> "int"
  | TFloat -> "float"
  | TString -> "string"
  | TBool -> "bool"
  | TUnit -> "unit"
  | TArrow -> "arrow"
  | TyFun (_, _) -> "fun"


let string_of_type = function 
    | App(tycon, _) -> string_of_tycon tycon
    | _ -> ""      