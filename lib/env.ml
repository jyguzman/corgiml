open Ast

type binding = 
  | VarBind of Ast.ty

type env = (string * binding) list
type tenv = (string * ty) list

module Map = Map.Make(String)
  
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

module Type_env = struct 
  type t = (ty Map.t) list

  let add (maps: t) bindings = 
    (Map.of_seq @@ List.to_seq bindings) :: maps
   
  let rec lookup name (maps: t): ty option = 
    match maps with 
      [] -> None 
    | curr :: rest -> 
      match Map.find_opt name curr with 
        Some ty -> Some ty 
      | None -> lookup name rest
end 