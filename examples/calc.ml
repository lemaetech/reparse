(** An implementation of a calculator.

    The expression grammar is defined by the following BNF grammar:

    {v
<expr>   ::= <term>   "+" <expr> 
           | <term>
<term>   ::= <factor> "*" <term> 
           | <factor>
<factor> ::= "(" <expr> ")" 
           | integer
    v}

    Sample toplevel (utop/ocaml) inputs:

    {v
      parse "123";;
      parse "123 - 123";;
      parse "123+123";;
      parse "123*123";;
      parse "123*123 - 23*234";;
    v} *)

module P = Reparse.Parser
open P.Infix

type expr =
  | Int  of int
  | Add  of expr * expr
  | Sub  of expr * expr
  | Mult of expr * expr
  | Div  of expr * expr

let skip_spaces = P.skip P.space

let binop : 'a P.t -> char -> 'b P.t -> ('a -> 'b -> 'c) -> 'c P.t =
 fun exp1 op exp2 f ->
  P.map3
    (fun e1 _ e2 -> f e1 e2)
    exp1
    (skip_spaces *> P.char op <* skip_spaces)
    exp2

let integer : expr P.t =
  let+ d = P.digits in
  Int (int_of_string d)

let factor : expr P.t -> expr P.t =
 fun expr ->
  P.any
    [ P.char '(' *> skip_spaces *> expr <* skip_spaces <* P.char ')'
    ; skip_spaces *> integer <* skip_spaces ]

let term : expr P.t -> expr P.t =
 fun factor ->
  P.fix (fun term ->
      let mult = binop factor '*' term (fun e1 e2 -> Mult (e1, e2)) in
      let div = binop factor '/' term (fun e1 e2 -> Div (e1, e2)) in
      mult <|> div <|> factor)

let expr : expr P.t =
  P.fix (fun expr ->
      let factor = factor expr in
      let term = term factor in
      let add = binop term '+' expr (fun e1 e2 -> Add (e1, e2)) in
      let sub = binop term '-' expr (fun e1 e2 -> Sub (e1, e2)) in
      P.any [add; sub; term])

let rec eval : expr -> int = function
  | Int i         -> i
  | Add (e1, e2)  -> eval e1 + eval e2
  | Sub (e1, e2)  -> eval e1 - eval e2
  | Mult (e1, e2) -> eval e1 * eval e2
  | Div (e1, e2)  -> eval e1 / eval e2

let parse : string -> expr =
 fun s ->
  let input = new P.string_input s in
  P.parse input expr

(* Test AST *)
let r =
  let actual = parse "1*2-4+3" in
  let expected = Sub (Mult (Int 1, Int 2), Add (Int 4, Int 3)) in
  Bool.equal (expected = actual) true

(* Run the evaluator. *)
let exp_result = eval (parse "12+1*10") |> Int.equal 22

(*-------------------------------------------------------------------------
 * Copyright (c) 2020 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * %%NAME%% %%VERSION%%
 *-------------------------------------------------------------------------*)
