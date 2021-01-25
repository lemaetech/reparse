## Reparse

Reparse is a monadic, recursive descent based, comprehensive parser construction library for ocaml.

[Reparse Documentation](https://lemaetech.co.uk/reparse/)

## Getting Started

```sh
$ opam install reparse
```

Add `reparse` to dune,

```
(executable # or library
  (name hello_world)
  (public_name hello_world)
  (libraries reparse))
```

## Example - Calculator

A **calculator** is the `hello world` of parsers. Here is an implementation in `Reparse` which supports `+,-,*` and `/` operations.

```ocaml
open Reparse

type expr =
  | Int of int
  | Add of expr * expr
  | Sub of expr * expr
  | Mult of expr * expr
  | Div of expr * expr

let skip_spaces = skip (ignore_m space)

let binop : 'a t -> char -> 'b t -> ('a -> 'b -> 'c) -> 'c t =
 fun exp1 op exp2 f ->
  map3 exp1 (skip_spaces *> char op <* skip_spaces) exp2 ~f:(fun e1 _ e2 -> f e1 e2)
;;

let integer : expr t =
  let+ d = digits in
  Int (int_of_string d)
;;

let factor : expr t -> expr t =
 fun expr ->
  any
    [ char '(' *> skip_spaces *> expr <* skip_spaces <* char ')'
    ; skip_spaces *> integer <* skip_spaces
    ]
;;

let term : expr t -> expr t =
 fun factor ->
  recur (fun term ->
      let mult = binop factor '*' term (fun e1 e2 -> Mult (e1, e2)) in
      let div = binop factor '/' term (fun e1 e2 -> Div (e1, e2)) in
      mult <|> div <|> factor)
;;

let expr : expr t =
  recur (fun expr ->
      let factor = factor expr in
      let term = term factor in
      let add = binop term '+' expr (fun e1 e2 -> Add (e1, e2)) in
      let sub = binop term '-' expr (fun e1 e2 -> Sub (e1, e2)) in
      any [ add; sub; term ])
;;

let rec eval : expr -> int = function
  | Int i -> i
  | Add (e1, e2) -> eval e1 + eval e2
  | Sub (e1, e2) -> eval e1 - eval e2
  | Mult (e1, e2) -> eval e1 * eval e2
  | Div (e1, e2) -> eval e1 / eval e2
;;

(* Test AST *)
let r =
  let actual = parse_string expr "1*2-4+3" in
  let expected = Sub (Mult (Int 1, Int 2), Add (Int 4, Int 3)) in
  Bool.equal (expected = actual) true
;;

(* Run and test the evaluator. *)
let exp_result =
  let v = eval (parse_string expr "12+1*10") in
  Int.equal 22 v
;;

```

The expression grammar is defined by the following BNF grammar:

```ebnf
<expr>   ::= <term>   "+" <expr>
           | <term>
<term>   ::= <factor> "*" <term>
           | <factor>
<factor> ::= "(" <expr> ")"
           | integer
```

## More Examples

- [HTTP Multipart](https://github.com/lemaetech/http-multipart-formdata/blob/master/src/http_multipart_formdata.ml)
- [RFC 8259 JSON parser](https://github.com/lemaetech/reparse/blob/master/examples/json.ml)
- [HTML5 parser](https://github.com/lemaetech/pp_html/blob/master/src/pp_html.ml)

## Credits

- https://github.com/MLton/mlton/blob/master/lib/mlton/basic/parse.sig
- https://github.com/c-cube/ocaml-containers/blob/master/src/core/CCParse.mli
