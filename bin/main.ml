open Corgiml
(* open Token  *)

(* type source = {
  raw: string;
  lines: string list;
  map: (int, int) Hashtbl.t;
  tokens: token list
} *)

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

(* module TypeChecker = Typecheck.TypeChecker(Formatter);; *)

(* let p = Parse.parse source_info.raw in  *)

let typ = Parse.parse_type "((Bool)" in 
  match typ with 
      Ok t -> print_endline (Ast.string_of_type t)
    | Error e -> (match e with 
        Parse.Unexpected_eof s -> print_endline s
      | Parse.Unexpected_token s -> print_endline s
      | Parse.Invalid_rec_let_binding s -> print_endline s
      | Parse.Unsupported_type s -> print_endline s)

(* in
match p with 
    Ok p -> print_endline (Ast.stringify_program p)
      (* let _ = print_endline (Ast.stringify_program p) in
      (match TypeChecker.check_module_item Env.init_type_env (List.hd p) with 
        Ok (_) -> print_endline "good" 
      | Error e -> match e with 
          Typecheck.Type_mismatch e -> print_endline e
        | Typecheck.Unrecognized_operation e -> print_endline e) *)
  | Error e -> (match e with 
      Parse.Unexpected_eof s -> print_endline s
    | Parse.Unexpected_token s -> print_endline s
    | Parse.Invalid_rec_let_binding s -> print_endline s
    | Parse.Unsupported_type s -> print_endline s)  *)




