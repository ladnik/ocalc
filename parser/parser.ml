open Lexer

type 'a expr = Value of 'a | BinOp of 'a token * 'a expr * 'a expr

module Parser : sig
  val parse : float token list -> float expr
  val ast_to_graphviz : string -> float expr -> unit
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
          "Syntax error: Expected highest precedence: number or parenthesis. \
           Unary operators are not supported."

  and parse_high_precedence tokens =
    let lhs, remaining = parse_highest_precedence tokens in
    match remaining with
    (*if there is a symbol of high precedence, parse the right hand
      side and construct a new abstract syntax tree node. As there
      is only one possible operator, we do not need to worry too
      much about left/right associativity*)
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
    (*operators are left associative, if not considered,
      1-2+3 would be interpreted as 1-(2+3). We therefore
      update th left hand side as long as there is an operator
      of the same precedence
    *)
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

  let string_of_node id = function
    | Value n ->
        {|"[|} ^ string_of_int id ^ "] VALUE " ^ string_of_float n ^ {|"|}
    | BinOp (Plus, _, _) -> {|"[|} ^ string_of_int id ^ "] PLUS (+)" ^ {|"|}
    | BinOp (Minus, _, _) -> {|"[|} ^ string_of_int id ^ "] MINUS (-)" ^ {|"|}
    | BinOp (Mul, _, _) -> {|"[|} ^ string_of_int id ^ "] MUL (*)" ^ {|"|}
    | BinOp (Div, _, _) -> {|"[|} ^ string_of_int id ^ "] DIV (/)" ^ {|"|}
    | BinOp (Mod, _, _) -> {|"[|} ^ string_of_int id ^ "] MOD (%)" ^ {|"|}
    | BinOp (Pow, _, _) -> {|"[|} ^ string_of_int id ^ "] POW (^)" ^ {|"|}
    | _ -> failwith "Invalid token in string_of_token"

  let ast_to_graphviz filename tree =
    let file = open_out filename in
    Printf.fprintf file "%s" "digraph ast { \n";
    Printf.fprintf file "%s" "node [shape = circle];\n";
    Printf.fprintf file "%s" "node [width=1.5];\n";
    let rec helper id node =
      match node with
      | Value _ ->
          Printf.fprintf file "%s" (string_of_node id node ^ "\n");
          ("", id, id + 1)
      | BinOp (_, l, r) ->
          let left_edges, left_id, n_id = helper id l in
          let right_edges, right_id, parent_id = helper n_id r in
          Printf.fprintf file "%s" (string_of_node parent_id node);
          let par_edges =
            string_of_node parent_id node
            ^ "->" ^ string_of_node left_id l ^ ";"
            ^ string_of_node parent_id node
            ^ "->" ^ string_of_node right_id r ^ ";"
          in
          let edge_str = left_edges ^ right_edges ^ par_edges ^ "\n" in
          (edge_str, parent_id, parent_id + 1)
    in
    let edge_str, _, _ = helper 0 tree in
    Printf.fprintf file "%s" edge_str;
    Printf.fprintf file "%s" "\n}";
    close_out file
end
