(* Implement JSON parser as defined at https://tools.ietf.org/html/rfc8259. 
  
  Assumes UTF-8 character encoding. However, it doesn't do any validation.

  Sample top_level inputs;

  {v 
  parse json_value "true";;
  parse json_value "false";;
  parse json_value "null";;
  parse json_value "123";;
  parse json_value "123.345";;
  parse json_value "123e123";;
  parse json_value "123.33E123";;
  parse json_value {|{"field1": 123,"field2": "value2"}|};;
  parse json_value {|{"field1":[123,"hello",-123.23], "field2":123} |};;
  parse json_value {|{"field1":123, "field2":123} |};;
  parse json_value {|[123,"hello",-123.23, 123.33e13, 123E23] |};;
  v}
 *)

module P = Reparse.Parser
open P.Infix

type value =
  | Object of (string * value) list
  | Array  of value list
  | Number of
      {negative: bool; int: string; frac: string option; exponent: string option}
  | String of string
  | False
  | True
  | Null

let ws =
  P.skip (P.satisfy (function ' ' | '\t' | '\n' | '\r' -> true | _ -> false))

let implode l = List.to_seq l |> String.of_seq
let struct_char c = ws *> P.char c <* ws
let null_value = ws *> P.string "null" *> ws *> P.pure Null
let false_value = ws *> P.string "false" *> ws *> P.pure False
let true_value = ws *> P.string "true" *> ws *> P.pure True
let sprintf = Printf.sprintf

let number_value =
  let* negative =
    P.optional (P.char '-') >|= function Some '-' -> true | _ -> false
  in
  let* int =
    let digits1_to_9 = P.satisfy (function '1' .. '9' -> true | _ -> false) in
    let num =
      P.map2
        (fun first_ch digits -> sprintf "%c%s" first_ch digits)
        digits1_to_9 P.digits in
    P.any [P.string "0"; num]
  in
  let* frac = P.optional (P.char '.' *> P.digits) in
  let+ exponent =
    P.optional
      (let* e = P.char 'E' <|> P.char 'e' in
       let* sign = P.optional (P.char '-' <|> P.char '+') in
       let sign = match sign with Some c -> sprintf "%c" c | None -> "" in
       let+ digits = P.digits in
       sprintf "%c%s%s" e sign digits)
  in
  Number {negative; int; frac; exponent}

let string =
  let escaped =
    let ch =
      P.char '\\'
      *> P.satisfy (function
           | '"' | '\\' | '/' | 'b' | 'f' | 'n' | 'r' | 't' -> true
           | _ -> false)
      >|= sprintf "\\%c" in
    let hex4digit =
      let+ hex =
        P.string "\\u" *> P.take ~at_least:4 ~up_to:4 P.hex_digit >|= implode
      in
      sprintf "\\u%s" hex in
    P.any [ch; hex4digit] in
  let unescaped =
    P.take_while
      ~while_:(P.is_not (P.any [P.char '\\'; P.control; P.dquote]))
      P.next
    >|= implode in
  let+ str = P.dquote *> P.take (P.any [escaped; unescaped]) <* P.dquote in
  String.concat "" str

let string_value = string >|= fun s -> String s

let json_value =
  P.fix (fun value ->
      let value_sep = struct_char ',' in
      let object_value =
        let member =
          let* nm = string <* struct_char ':' in
          let+ v = value in
          (nm, v) in
        let+ object_value =
          struct_char '{' *> P.take member ~sep_by:value_sep <* struct_char '}'
        in
        Object object_value in
      let array_value =
        let+ vals =
          struct_char '[' *> P.take value ~sep_by:value_sep <* struct_char ']'
        in
        Array vals in
      P.any
        [ object_value; array_value; number_value; string_value; false_value
        ; true_value; null_value ])

let parse p s =
  let input = new P.string_input s in
  P.parse input p

(*-------------------------------------------------------------------------
  * Copyright (c) 2020 Bikal Gurung. All rights reserved.
  *
  * This Source Code Form is subject to the terms of the Mozilla Public
  * License,  v. 2.0. If a copy of the MPL was not distributed with this
  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
  *
  * %%NAME%% %%VERSION%%
  *-------------------------------------------------------------------------*)
