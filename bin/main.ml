open Corgiml
open Token

let load_source_file file_name = 
  let file = open_in file_name in 
  let rec load_source_file_aux file acc =
    try 
      let line = input_line file in 
        load_source_file_aux file (acc ^ line ^ "\n")
    with _ ->
      acc
  in 
    load_source_file_aux file "";;

let source = load_source_file "./test/test.cml" in

let tokens = Lexer.tokenize_source source in 

let _ = print_endline (stringify_tokens tokens) in 

let p = Parser.parse tokens in  

match p with 
    Ok p -> print_endline (Ast.stringify_program p)
  | Error e -> (match e with 
      Parser.Unexpected_eof s -> print_endline s
    | Parser.Unexpected_token s -> print_endline s
    | Parser.Invalid_rec_let_binding s -> print_endline s
    | Parser.Unsupported_type s -> print_endline s) 

