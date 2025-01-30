type binding = 
  | VarBind of Type.ty

type env = (string * binding) list

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

