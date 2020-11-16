(* Implement JSON parser as defined at https://tools.ietf.org/html/rfc8259. *)

module P = Reparse.Parser
open P.Infix

type value =
  | Object
  | Array
  | Number of
      { negative : bool
      ; int : string
      ; frac : string option
      ; exponent : string option }
  | String
  | False
  | True
  | Null

let ws =
  P.skip
    (P.satisfy (function
        | ' '
        | '\t'
        | '\n'
        | '\r' ->
            true
        | _ -> false))
  *> P.unit

let implode l = List.to_seq l |> String.of_seq
let struct_char c = ws *> P.char c <* ws
let null_value = ws *> P.string "null" <* ws
let false_value = ws *> P.string "false" <* ws
let true_value = ws *> P.string "true" <* ws
let sprintf = Printf.sprintf

let number_value =
  let* negative =
    P.optional (P.char '-')
    >|= function
    | Some '-' -> true
    | _        -> false
  in
  let* int =
    let digits1_to_9 =
      P.satisfy (function
          | '1' .. '9' -> true
          | _          -> false)
    in
    let num =
      P.map2
        (fun first_ch digits -> sprintf "%c%s" first_ch digits)
        digits1_to_9
        P.digits
    in
    P.any [P.string "0"; num]
  in
  let* frac = P.optional (P.char '.' *> P.digits) in
  let+ exponent =
    P.optional
      (let* e = P.char 'E' <|> P.char 'e' in
       let* sign = P.optional (P.char '-' <|> P.char '+') in
       let sign =
         match sign with
         | Some c -> sprintf "%c" c
         | None   -> ""
       in
       let+ digits = P.digits in
       sprintf "%c%s%s" e sign digits)
  in
  Number {negative; int; frac; exponent}

(* let object_value = struct_char '{' *)
