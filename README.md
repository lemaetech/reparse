## Reparse

Reparse is a monadic, recursive descent based, comprehensive parser construction library for ocaml.

[Reparse Documentation](https://lemaetech.co.uk/reparse/)

## Getting Started

```
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
# #require "reparse";;
```

```ocaml
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
```

```ocaml
# let ast = parse expr "1*2-4+3";;
val ast : (expr, string) result =
  Ok (Sub (Mult (Int 1, Int 2), Add (Int 4, Int 3)))

# eval @@ Result.get_ok (parse expr "12+1*10")
- : int = 22
```

## More Examples

- [HTTP Multipart](https://github.com/lemaetech/http-multipart-formdata/blob/master/src/http_multipart_formdata.ml)
- [RFC 8259 JSON parser](https://github.com/lemaetech/reparse/blob/master/examples/json.ml)
- [HTML5 parser](https://github.com/lemaetech/pp_html/blob/master/src/pp_html.ml)

