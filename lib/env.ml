open Ast

type binding = 
  | VarBind of Ast.ty

type env = (string * binding) list
type tenv = (string * ty) list

let add (env: env) string binding = 
  (string, binding) :: env

let rec get (env: env) name = 
  match env with 
    | [] -> None 
    | (ident, binding) :: env_rest -> 
      if name = ident then 
        Some binding 
      else 
        get env_rest name

module Tenv = struct 
  let add (env: tenv) string t = 
    (string, t) :: env
  
  let rec get (env: tenv) name: ty option = 
    match env with 
      | [] -> None 
      | (ident, binding) :: env_rest -> 
        if name = ident then 
          Some binding 
        else 
          get env_rest name
end 

let int_bin_op_type = App(TArrow, [int; App(TArrow, [int; int])])
let float_bin_op_type = App(TArrow, [float; App(TArrow, [float; float])])

let (init_type_env: env) = List.fold_left (fun env op -> (op, VarBind int_bin_op_type) :: env) [] ["+"; "-"; "*"; "/"] 
let _ = List.fold_left (fun env op -> (op, VarBind float_bin_op_type) :: env) [] ["+."; "-."; "*."; "/."] 
