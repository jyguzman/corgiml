open Ast 

let (let*) r f = match r with 
  Ok v -> f v 
| Error e -> Error e 

let template = "switch expr "
let check_match _m =
  Ok ()

let gen_match_case case = 
  let _pattern, _result = case.lhs, case.rhs in 
  let _js_case = "" in
  ""

let gen_match _scrutinee cases = 
  gen_match_case (List.hd cases)

