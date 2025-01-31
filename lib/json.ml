type json = 
  | JsonNumber of float
  | JsonBool of bool 
  | JsonString of string 
  | JsonArray of json list
  | JsonObject of (string * json) list

let rec string_of_json = function 
    JsonNumber jn -> string_of_float jn 
  | JsonBool b -> string_of_bool b
  | JsonString s -> s
  | JsonArray a -> "[\n\t" ^ (List.fold_left (fun acc x -> acc ^ string_of_json x ^ ",\n\td") "" a) ^ "]"
  | JsonObject (pairs) -> 
    let pairs = List.fold_left (fun acc (key, value) -> acc ^ Printf.sprintf "\"%s\": %s\n" key (string_of_json value)) "" pairs in
    Printf.sprintf "{\n\t%s\n}" pairs