type expr = 
  | Number of float 
  | String of string 
  | Boolean of bool 
  | Function of string list 

type stmt = 
  | Assign of string * expr  



