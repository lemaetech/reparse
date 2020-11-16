(** Parses the following BNF grammar into exp AST,

    <expr> ::= <term> "+" <expr> | <term>

    <term> ::= <factor> "*" <term> | <factor>

    <factor> ::= "(" <expr> ")" | <const>

    <const> ::= integer

    '123'

    '123 + 123'

    '123+123'

    '123*123'

    '123 + 123 * 234' *)

module P = Reparse.Parser
open P.Infix

let pf = Printf.printf

type expr =
  | Int  of int
  | Add  of expr * expr
  | Sub  of expr * expr
  | Mult of expr * expr
  | Div  of expr * expr

let skip_spaces = P.skip P.space

let binop exp1 op exp2 f =
  P.map3
    (fun e1 _ e2 -> f e1 e2)
    exp1
    (skip_spaces *> P.char op <* skip_spaces)
    exp2

let integer =
  let+ d = P.digits in
  Int (int_of_string d)

let factor expr =
  P.any
    [ P.char '(' *> skip_spaces *> expr <* skip_spaces <* P.char ')'
    ; skip_spaces *> integer <* skip_spaces ]

let term factor =
  P.fix (fun term ->
      let mult = binop factor '*' term (fun e1 e2 -> Mult (e1, e2)) in
      let div = binop factor '/' term (fun e1 e2 -> Div (e1, e2)) in
      mult <|> div <|> factor)

let expr =
  P.fix (fun expr ->
      let factor = factor expr in
      let term = term factor in
      let add = binop term '+' expr (fun e1 e2 -> Add (e1, e2)) in
      let sub = binop term '-' expr (fun e1 e2 -> Sub (e1, e2)) in
      P.any [add; sub; term])

let rec eval = function
  | Int i         -> i
  | Add (e1, e2)  -> eval e1 + eval e2
  | Sub (e1, e2)  -> eval e1 - eval e2
  | Mult (e1, e2) -> eval e1 * eval e2
  | Div (e1, e2)  -> eval e1 / eval e2

let parse s =
  let input = new P.string_input s in
  P.parse input expr

(* Test AST *)
let result (* true *) =
  let e1 = Sub (Mult (Int 1, Int 2), Add (Int 4, Int 3)) in
  let e2 = parse "1*2-4+3" in
  e1 = e2

(* Run the evaluator. *)
let exp_result = eval (parse "12+1*10") (* Should be 22. *)
