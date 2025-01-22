type ty = 
    | TNil 
    | App of tycon * ty list 
    | Var of tyvar 
    | Poly of tyvar list * ty

and tycon = 
    | TInt 
    | TString 
    | TUnit 
    | TArrow 
    | TyFun of tyvar list * ty

and tyvar = string
