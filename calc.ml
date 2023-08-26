open Lexer
open Parser
open OUnit2

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

let print_ast_to_file filename ast = Parser.ast_to_graphviz filename ast

let tests =
  "a"
  >::: [
         ( "[Lexer] Empty string sould lead to empty token list" >:: fun _ ->
           assert_equal [] (Lexer.string_to_token_list "") );
         ( "[Lexer] Ignore whitespace" >:: fun _ ->
           assert_equal
             [ Number 1.; Plus; Number 2.; Plus; Number 3. ]
             (Lexer.string_to_token_list "1 + 2 + 3") );
         ( "[Lexer] Invalid token ~" >:: fun _ ->
           assert_equal [ Invalid ] (Lexer.string_to_token_list "~") );
         ( "[Lexer] Invalid token #" >:: fun _ ->
           assert_equal [ Invalid ] (Lexer.string_to_token_list "#") );
         ( "[Lexer] Invalid token # in combination with valid symbols"
         >:: fun _ ->
           assert_equal [ Invalid ] (Lexer.string_to_token_list "1+5-4/5*0#") );
         ( "[Lexer] Invalid token .." >:: fun _ ->
           assert_equal [ Invalid ] (Lexer.string_to_token_list "1..") );
         ( "[Lexer] Float 1.2345" >:: fun _ ->
           assert_equal [ Number 1.2345 ] (Lexer.string_to_token_list "1.2345")
         );
         ( "[Lexer] Float 0.01" >:: fun _ ->
           assert_equal [ Number 0.01 ] (Lexer.string_to_token_list "0.01") );
         ( "[Lexer] Float 1234.5" >:: fun _ ->
           assert_equal [ Number 1234.5 ] (Lexer.string_to_token_list "1234.5")
         );
         ( "[Lexer] Float 1.2.3 should become 1.2 and 0.3" >:: fun _ ->
           assert_equal [ Number 1.2; Number 0.3 ]
             (Lexer.string_to_token_list "1.2.3") );
         ( "[Lexer] Float 3..4 should become 3.0 and 0.4" >:: fun _ ->
           assert_equal
             [
               Number 1.;
               LeftPar;
               Number 2.;
               LeftPar;
               Number 3.;
               Number 0.4;
               RightPar;
               RightPar;
             ]
             (Lexer.string_to_token_list "1(2(3..4))") );
         ( "[Lexer] Token list 1+2-3" >:: fun _ ->
           assert_equal
             [ Number 1.; Plus; Number 2.; Minus; Number 3. ]
             (Lexer.string_to_token_list "1+2-3") );
         ( "[Lexer] Token list 1*2/3" >:: fun _ ->
           assert_equal
             [ Number 1.; Mul; Number 2.; Div; Number 3. ]
             (Lexer.string_to_token_list "1*2/3") );
         ( "[Lexer] Token list 1%2^3" >:: fun _ ->
           assert_equal
             [ Number 1.; Mod; Number 2.; Pow; Number 3. ]
             (Lexer.string_to_token_list "1%2^3") );
         ( "[Lexer] Token list ()(())" >:: fun _ ->
           assert_equal
             [ LeftPar; RightPar; LeftPar; LeftPar; RightPar; RightPar ]
             (Lexer.string_to_token_list "()(())") );
         ( "[Eval] Precedence: 1 - 2 + 3" >:: fun _ ->
           assert_equal 2. (eval_string_expr "1-2+3") );
         ( "[Eval] Precedence: 1 - (2 + 3)" >:: fun _ ->
           assert_equal (-4.) (eval_string_expr "1-(2+3)") );
         ( "[Eval] Precedence: 2/3*3" >:: fun _ ->
           assert_equal 2. (eval_string_expr "2/3*3") );
         ( "[Eval] Precedence: 2+2^2" >:: fun _ ->
           assert_equal 6. (eval_string_expr "2+2^2") );
         ( "[Eval] Precedence: 2+2%2" >:: fun _ ->
           assert_equal 2. (eval_string_expr "2+2%2") );
         ( "[Eval] Correct value: 1-2*8/(4^2)*.5-0.4+6*3" >:: fun _ ->
           assert_equal 18.1 (eval_string_expr "1-2*8/(4^2)*.5-0.4+6*3") );
       ]

let main () =
  print_endline "";
  print_endline "Enter a string to evaulate or 't' to run tests";
  match read_line () with
  | "t" -> run_test_tt_main tests
  | str -> print_endline ("Result: " ^ string_of_float (eval_string_expr str))

let _ = main ()
