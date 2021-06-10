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

open Reparse.String

type expr =
  | Int of int
  | Add of expr * expr
  | Sub of expr * expr
  | Mult of expr * expr
  | Div of expr * expr

let skip_spaces = skip space

let binop : 'a t -> char -> 'b t -> ('a -> 'b -> 'c) -> 'c t =
 fun exp1 op exp2 f ->
  map3
    (fun e1 _ e2 -> f e1 e2)
    exp1
    (skip_spaces *> char op <* skip_spaces)
    exp2

let integer : expr t =
  let+ d = skip_spaces *> digits <* skip_spaces in
  Int (int_of_string d)

let factor : expr t -> expr t =
 fun expr ->
  any [ char '(' *> skip_spaces *> expr <* skip_spaces <* char ')'; integer ]

let term : expr t -> expr t =
 fun factor ->
  recur (fun term ->
      let mult = binop factor '*' term (fun e1 e2 -> Mult (e1, e2)) in
      let div = binop factor '/' term (fun e1 e2 -> Div (e1, e2)) in
      mult <|> div <|> factor)

let expr : expr t =
  recur (fun expr ->
      let factor = factor expr in
      let term = term factor in
      let add = binop term '+' expr (fun e1 e2 -> Add (e1, e2)) in
      let sub = binop term '-' expr (fun e1 e2 -> Sub (e1, e2)) in
      any [ add; sub; term ] <?> "expr")

let rec eval : expr -> int = function
  | Int i -> i
  | Add (e1, e2) -> eval e1 + eval e2
  | Sub (e1, e2) -> eval e1 - eval e2
  | Mult (e1, e2) -> eval e1 * eval e2
  | Div (e1, e2) -> eval e1 / eval e2

(* Test AST *)
let r =
  let actual = parse expr (of_string "1*2-4+3") in
  let expected = Ok (Sub (Mult (Int 1, Int 2), Add (Int 4, Int 3))) in
  Bool.equal (expected = actual) true

(* Run and test the evaluator. *)
let exp_result =
  let v = eval @@ Result.get_ok (parse expr @@ of_string "12+1*10") in
  Int.equal 22 v

(*-------------------------------------------------------------------------
 * Copyright (c) 2020 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * %%NAME%% %%VERSION%%
 *-------------------------------------------------------------------------*)
