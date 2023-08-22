open Lexer
open Parser

let generate_token_list = Lexer.string_to_token_list
let generate_syntax_tree li = Parser.parse li

let get_binop = function
  | Plus -> ( +. )
  | Minus -> ( -. )
  | Mul -> ( *. )
  | Div -> ( /. )
  | Pow -> ( ** )
  | Mod -> mod_float
  | _ -> failwith "Invalid operator in AST"

let rec eval_syntax_tree = function
  | Value v -> v
  | BinOp (op, a, b) -> (get_binop op) (eval_syntax_tree a) (eval_syntax_tree b)

let eval_string_expr str =
  let li = generate_token_list str in
  let ast = generate_syntax_tree li in
  eval_syntax_tree ast
