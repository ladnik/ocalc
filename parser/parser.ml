open Lexer

type 'a expr = Value of 'a | BinOp of 'a token * 'a expr * 'a expr

module Parser : sig
  val parse : float token list -> float expr
  val ast_to_graphviz : float expr -> string
end = struct
  (*use a recursive descent parser*)
  (*https://en.wikipedia.org/wiki/Recursive_descent_parser*)
  (*http://craftinginterpreters.com/parsing-expressions.html*)

  let rec parse_highest_precedence tokens =
    (*we reached highest precedence, therefore we do not have to
       parse left or right*)
    match tokens with
    | Number n :: tl -> (Value n, tl)
    | LeftPar :: tl -> (
        let inner, remaining = parse_low_precedence tl in
        match remaining with
        | RightPar :: tl' -> (inner, tl')
        | _ -> failwith "Syntax error: Expected closing parenthesis")
    | _ ->
        failwith
          "Syntax error: Expected highest precedence: number or parenthesis"

  and parse_high_precedence tokens =
    let lhs, remaining = parse_highest_precedence tokens in
    match remaining with
    (*if there is a symbol of high precedence, parse the right hand
      side and construct a new abstract syntax tree node*)
    | Pow :: tl ->
        let rhs, remaining_rhs = parse_high_precedence tl in
        (BinOp (Pow, lhs, rhs), remaining_rhs)
    | _ -> (lhs, remaining)

  and parse_medium_precedence tokens =
  (*if there is a symbol of medium precedence, parse the right hand
      side and construct a new abstract syntax tree node*)
    let lhs, remaining = parse_high_precedence tokens in
    parse_medium_precedence_helper lhs remaining

  and parse_medium_precedence_helper lhs tokens =
  (*operators are left associative,without considering this, 
     1-2+3 would be interpreted as 1-(2+3). We therefore 
     update th left hand side as long as there is an operator
     of the same precedence
     *)
    match tokens with
    | Mul :: tl ->
        let rhs, remaining_rhs = parse_high_precedence tl in
        let new_lhs = BinOp (Mul, lhs, rhs) in
        parse_medium_precedence_helper new_lhs remaining_rhs
    | Div :: tl ->
        let rhs, remaining_rhs = parse_high_precedence tl in
        let new_lhs = BinOp (Div, lhs, rhs) in
        parse_medium_precedence_helper new_lhs remaining_rhs
    | Mod :: tl ->
        let rhs, remaining_rhs = parse_high_precedence tl in
        let new_lhs = BinOp (Mod, lhs, rhs) in
        parse_medium_precedence_helper new_lhs remaining_rhs
    | _ -> (lhs, tokens)

  and parse_low_precedence tokens =
  (*if there is a symbol of low precedence, parse the right hand
      side and construct a new abstract syntax tree node*)
    let lhs, remaining = parse_medium_precedence tokens in
    parse_low_precedence_helper lhs remaining

  and parse_low_precedence_helper lhs tokens =
    match tokens with
    | Plus :: tl ->
        let rhs, remaining_rhs = parse_medium_precedence tl in
        let new_lhs = BinOp (Plus, lhs, rhs) in
        parse_low_precedence_helper new_lhs remaining_rhs
    | Minus :: tl ->
        let rhs, remaining_rhs = parse_medium_precedence tl in
        let new_lhs = BinOp (Minus, lhs, rhs) in
        parse_low_precedence_helper new_lhs remaining_rhs
    | _ -> (lhs, tokens)

  let parse tokens =
    let tree, remaining = parse_low_precedence tokens in
    match remaining with
    | [] -> tree (*we constructed a valid ast*)
    | _ -> failwith "token list couldn't be fully parsed"

  let ast_to_graphviz _ =
    (*TODO*)
    let str = "" in
    "digraph ast {" ^ str ^ "}"
end
