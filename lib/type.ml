type ty = 
    | Nil 
    | App of tycon * ty list 
    | Var of tyvar 
    | Poly of tyvar list * ty

and tycon = 
    | TInt 
    | TFloat
    | TString 
    | TBool
    | TUnit 
    | TArrow 
    | TyFun of tyvar list * ty

and tyvar = string
