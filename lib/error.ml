type err = 
  | Type_mismatch of Ast.expr * string

type error = {
  source: string;
  line: int;
  col: int;
  error: err
}

type context = {
  source: string ref;
  lines: (int, int) Hashtbl.t ref
}

let range first last = 
  let rec range_aux acc = 
    let curr = List.hd acc in 
    if curr = last then 
      acc 
    else 
      range_aux ((curr + 1) :: acc)
  in 
    List.rev (range_aux [first])

module Formatter = struct 
  let display_line line = 
    let line_num, src_line = line in 
      Printf.sprintf "%d  |\t%s" line_num src_line
end 

module ErrorReporter = struct
  let ctxt = {
    source = ref "";
    lines = ref (Hashtbl.create 1)
  }

  let init source lines = 
    let _ = ctxt.source := source in
    ctxt.lines := lines
  
  let src () = !(ctxt.source)

  let lines () = !(ctxt.lines)

  let get_src_line line_num = 
    let start_pos = Hashtbl.find (lines ()) line_num in 
    let last_pos = Hashtbl.find (lines ()) (line_num + 1) in 
      line_num, String.sub (src ()) start_pos (last_pos - start_pos - 1)

  let get_src_lines first last = 
    List.rev (List.fold_left (fun lines line_num -> (line_num, get_src_line line_num) :: lines) [] (range first last))
  
  let display_line line_num = 
    let line = get_src_line line_num in 
      Formatter.display_line line
end


