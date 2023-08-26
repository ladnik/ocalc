type 'a token =
  | Number of 'a
  | Plus
  | Minus
  | Mul
  | Div
  | Pow
  | Mod
  | LeftPar
  | RightPar
  (* | Min
     | Max *)
  | Invalid

module Lexer : sig
  val string_to_token_list : string -> float token list
end = struct
  let isDigit = function '0' .. '9' -> true | _ -> false
  let charListToString li = String.of_seq (List.to_seq li)

  let rec digitCons s idx acc =
    (*read floating point value from string and return index of first char not belonging to this number, as well as the constructed float string*)
    if idx >= String.length s then (List.rev acc, idx)
    else
      let c = String.get s idx in
      if isDigit c then digitCons s (idx + 1) (c :: acc)
      else if c = '.' then
        if List.mem '.' acc then (List.rev acc, idx)
          (*if there already is a decimal point in the current read substring, stop reading*)
        else digitCons s (idx + 1) (c :: acc)
      else (List.rev acc, idx)
  (*the current character is neither a digit, nor a dot - stop reading*)

  let readDigit s idx =
    (*read a digit in the given string, starting at index idx**)
    let float_string, end_idx = digitCons s idx [] in
    match float_of_string_opt (charListToString float_string) with
    | None -> None
    | Some v -> Some (v, end_idx - idx)

  (*recursively read in all tokens*)
  let string_to_token_list s =
    (*TODO: remove whitespace*)
    let rec helper token_acc cur_idx =
      if cur_idx >= String.length s then token_acc
      else
        match String.get s cur_idx with
        | '+' -> helper (Plus :: token_acc) (cur_idx + 1)
        | '-' -> helper (Minus :: token_acc) (cur_idx + 1)
        | '*' -> helper (Mul :: token_acc) (cur_idx + 1)
        | '/' -> helper (Div :: token_acc) (cur_idx + 1)
        | '^' -> helper (Pow :: token_acc) (cur_idx + 1)
        | '%' -> helper (Mod :: token_acc) (cur_idx + 1)
        | '(' -> helper (LeftPar :: token_acc) (cur_idx + 1)
        | ')' -> helper (RightPar :: token_acc) (cur_idx + 1)
        (* | 'm' ->
            if String.length s - cur_idx < 3 then Invalid :: token_acc
            else if
              String.get s (cur_idx + 1) = 'i'
              && String.get s (cur_idx + 2) = 'n'
            then helper (Min :: token_acc) (cur_idx + 3)
            else if
              String.get s (cur_idx + 1) = 'a'
              && String.get s (cur_idx + 2) = 'x'
            then helper (Max :: token_acc) (cur_idx + 3)
            else Invalid :: token_acc *)
        | _ -> (
            match readDigit s cur_idx with
            | None -> [Invalid]
            | Some (n, shift) -> helper (Number n :: token_acc) (cur_idx + shift)
            )
    in
    (*finally, reverse the token list*)
    List.rev (helper [] 0)
end
