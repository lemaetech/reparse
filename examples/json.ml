(* Implement JSON parser as defined at https://tools.ietf.org/html/rfc8259.

   Assumes UTF-8 character encoding. However, it doesn't do any validation.
   
   Note: It is unknown if the parser fully conforms to RFC 8259 as no testing, 
   validation is done. The RFC is used mainly as a guidance and the sample is 
   meant to demonstrate parser construction using reparse rather than a production
   grade parser.
   
   Sample top_level inputs;

   {v parse "true";; parse "false";; parse "null";; parse "123";; parse
   "123.345";; parse "123e123";; parse "123.33E123";; parse {|{"field1":
   123,"field2": "value2"}|};; parse {|{"field1":[123,"hello",-123.23],
   "field2":123} |};; parse {|{"field1":123, "field2":123} |};; parse
   {|[123,"hello",-123.23, 123.33e13, 123E23] |};; v} *)

open Reparse

type value =
  | Object of (string * value) list
  | Array of value list
  | Number of
      { negative : bool
      ; int : string
      ; frac : string option
      ; exponent : string option
      }
  | String of string
  | False
  | True
  | Null

(*---- utils and aliases ----*)
let sprintf = Printf.sprintf

let implode l = List.to_seq l |> String.of_seq

(*---- Parser definitions ---------*)
let ws =
  skip
    (char_if (function
      | ' '
      | '\t'
      | '\n'
      | '\r' ->
        true
      | _ -> false))

let struct_char c = ws *> char c <* ws

let null_value = ws *> string "null" *> ws *> return Null

let false_value = ws *> string "false" *> ws *> return False

let true_value = ws *> string "true" *> ws *> return True

let number_value =
  let* negative =
    optional (char '-') >>| function
    | Some '-' -> true
    | _ -> false
  in
  let* int =
    let digits1_to_9 =
      char_if (function
        | '1' .. '9' -> true
        | _ -> false)
    in
    let num =
      map2
        ~f:(fun first_ch digits -> sprintf "%c%s" first_ch digits)
        digits1_to_9 digits
    in
    any [ string "0"; num ]
  in
  let* frac = optional (char '.' *> digits) in
  let+ exponent =
    optional
      (let* e = char 'E' <|> char 'e' in
       let* sign = optional (char '-' <|> char '+') in
       let sign =
         match sign with
         | Some c -> sprintf "%c" c
         | None -> ""
       in
       let+ digits = digits in
       sprintf "%c%s%s" e sign digits)
  in
  Number { negative; int; frac; exponent }

let string =
  let escaped =
    let ch =
      char '\\'
      *> char_if (function
           | '"'
           | '\\'
           | '/'
           | 'b'
           | 'f'
           | 'n'
           | 'r'
           | 't' ->
             true
           | _ -> false)
      >>| sprintf "\\%c"
    in
    let hex4digit =
      let+ hex =
        string "\\u" *> take ~at_least:4 ~up_to:4 hex_digit >>| implode
      in
      sprintf "\\u%s" hex
    in
    any [ ch; hex4digit ]
  in
  let unescaped =
    take_while ~while_:(is_not (any [ char '\\'; control; dquote ])) next
    >>| implode
  in
  let+ str = dquote *> take (any [ escaped; unescaped ]) <* dquote in
  String.concat "" str

let string_value = string >>| fun s -> String s

let json_value =
  recur (fun value ->
      let value_sep = struct_char ',' in
      let object_value =
        let member =
          let* nm = string <* struct_char ':' in
          let+ v = value in
          (nm, v)
        in
        let+ object_value =
          struct_char '{' *> take member ~sep_by:value_sep <* struct_char '}'
        in
        Object object_value
      in
      let array_value =
        let+ vals =
          struct_char '[' *> take value ~sep_by:value_sep <* struct_char ']'
        in
        Array vals
      in
      any
        [ object_value
        ; array_value
        ; number_value
        ; string_value
        ; false_value
        ; true_value
        ; null_value
        ])

let parse s = parse_string json_value s

(*------------------------------------------------------------------------- *
  Copyright (c) 2020 Bikal Gurung. All rights reserved. * * This Source Code
  Form is subject to the terms of the Mozilla Public * License, v. 2.0. If a
  copy of the MPL was not distributed with this * file, You can obtain one at
  https://mozilla.org/MPL/2.0/. * * %%NAME%% %%VERSION%%
  *-------------------------------------------------------------------------*)
