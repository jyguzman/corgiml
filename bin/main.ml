open Corgiml

let load_source file_name = 
  let file = open_in file_name in 
  let rec load_source_aux file acc =
    try 
      let line = input_line file in 
      load_source_aux file (acc ^ line ^ "\n")
    with _ ->
      String.sub acc 0 (String.length acc - 1)
  in 
  load_source_aux file ""

let source = load_source "./test/test.cml"

let source_info = Lexer.tokenize_source source

module Formatter = Error.Formatter(struct let src = source_info end);;

let p = Parse.parse source_info.raw in 

match p with 
    Ok p -> 
      let _ = print_endline (Ast.stringify_program p) in  
      let first = List.hd p in 
      let str = Js_codegen.Codegen.js_of_mi first in 
      (match str with 
        Ok s -> print_endline s 
      | Error s -> print_endline ("Error: " ^ s))
  | Error e -> (match e with 
      Parse.Unexpected_eof s
    | Parse.Unexpected_token s
    | Parse.Invalid_rec_let_binding s
    | Parse.Unsupported_type s -> print_endline s) 




