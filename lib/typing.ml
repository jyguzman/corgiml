type typ =
  | TInt 
  | TFloat
  | TBool
  | TVar of typ_var

and typ_var = {
  name: string;
  parent: typ option ref;
}

type type_error = 
  | Type_error of string
  | Occurs_in of string

let rec find_type_of typ = match typ with 
  TBool | TFloat | TInt -> typ
  | TVar t ->
  (match !(t.parent) with 
      None -> typ
    | Some parent -> 
      let root = find_type_of parent in
      if root != parent then 
        t.parent := Some root; 
      root)

let set_equal_types t1 t2 = 
  let root_t1 = find_type_of t1 in 
  let root_t2 = find_type_of t2 in 
  match root_t1, root_t2 with 
    TVar t1, TVar t2 when t1 != t2 -> 
      t1.parent := Some root_t2
    | _ -> ()

let rec occurs_in t1 t2 =   
  match t1, t2 with 
    TVar tv1, TVar tv2 -> 
      if tv1 == tv2 then true else 
      (match !(tv2.parent) with 
        None -> false 
      | Some tv2_parent ->  occurs_in t1 tv2_parent)
  | _ -> false


let rec stringify_type typ = match typ with 
  TVar t -> 
    let inner = !(t.parent) in 
    let inner_str = (match inner with 
      None -> "None"
    | Some p -> stringify_type p) 
    in
    Printf.sprintf "TVar{name = %s; parent = %s}" t.name inner_str 
  | TInt -> "TInt" | TFloat -> "TFloat" | TBool -> "TBool"


let (typ_map: (string, typ) Hashtbl.t) = Hashtbl.create 32

let fresh_tyvar name = { name = name; parent = ref None }

let unify t1 t2 = 
  let t1_root = find_type_of t1 in 
  let t2_root = find_type_of t2 in 
  match t1_root, t2_root with 
    TVar _, TVar _ -> 
      if occurs_in t1_root t2_root then 
        Error (Occurs_in (Printf.sprintf "cannot unify: type_var %s occurs in type_var %s" (stringify_type t1) (stringify_type t2))) 
      else 
        let _ = set_equal_types t1_root t2_root in Ok ()

    | TVar _, _ -> 
      let _ = set_equal_types t1_root t2_root in Ok ()

    | _, TVar _ -> 
      let _ = set_equal_types t2_root t1_root in Ok ()

    | _, _ -> 
      if t1_root <> t2_root then 
        Error (Type_error (Printf.sprintf "unify: unequal concrete types %s and %s" (stringify_type t1) (stringify_type t2)))
      else 
        Ok ()

(* let infer expr env = 
  let result = fresh_tyvar "'a" in 
  match expr with 
    Ast.Integer _ -> 
      let _ = unify result TInt in Ok ()
    | _ -> Error (Printf.sprintf "unexpected expr type %s" (Ast.stringify_expr expr)) *)