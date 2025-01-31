open Corgiml
(* open Token
open Error *)

let load_source file_name = 
  let file = open_in file_name in 
  let rec load_source_aux file acc =
    try 
      let line = input_line file in 
      load_source_aux file (acc ^ line ^ "\n")
    with _ ->
      String.sub acc 0 (String.length acc - 1)
  in 
  load_source_aux file "";;

let source = load_source "./test/test.cml";;

let source_info = Lexer.tokenize_source source;;

let _ = print_endline (Json.string_of_json json);;

module Formatter = Error.Formatter(struct let src = source_info end);;

module TypeChecker = Typecheck.TypeChecker(Formatter);;

let p = Parser.parse source_info.tokens in 

match p with 
    Ok p -> 
      let _ = print_endline (Ast.stringify_program p) in
      (match TypeChecker.check_module_item (List.hd p) [] with 
        Ok (_) -> print_endline "good" 
      | Error e -> match e with 
          Typecheck.Type_mismatch e -> print_endline e
        | Typecheck.Unrecognized_operation e -> print_endline e)
  | Error e -> (match e with 
      Parser.Unexpected_eof s -> print_endline s
    | Parser.Unexpected_token s -> print_endline s
    | Parser.Invalid_rec_let_binding s -> print_endline s
    | Parser.Unsupported_type s -> print_endline s) 




