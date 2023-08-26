open Lexer 
open Parser

val generate_token_list : string -> float token list
val generate_syntax_tree : float token list -> float expr
val eval_syntax_tree : float expr -> float
val eval_string_expr : string -> float

