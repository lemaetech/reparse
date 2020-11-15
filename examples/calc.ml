(** Parses the following into exp AST,

    '123'

    '123 + 123'

    '123+123'

    '123*123'

    '123 + 123 * 234' *)

module P = Reparse.Parser
open P.Infix

(** Integer based binary math expression. *)
type math_exp =
  | Int    of int
  | Bin_op of math_exp * binary_op * math_exp

(** Binary operation *)
and binary_op =
  | Plus
  | Mult

let parse_int =
  let+ d = P.digits in
  Int (int_of_string d)

let skip_spaces = P.skip P.space

let parse_binop =
  let+ op =
    skip_spaces *> (P.char '+' <|> P.char '*')
    <* skip_spaces
    >>= (function
          | '+' -> P.pure Plus
          | '*' -> P.pure Mult
          | c   -> P.fail (Printf.sprintf "unsupported operation: %c" c))
    |> P.optional
  in
  op

let rec parse_exp () =
  let* exp1 = parse_int in
  let* op = parse_binop in
  match op with
  | Some bin_op ->
      let+ exp2 = parse_exp () in
      Bin_op (exp1, bin_op, exp2)
  | None        -> P.pure exp1

let parse s =
  let input = new P.string_input s in
  P.parse input (parse_exp ())

(** Evaluate expression. *)
let rec eval = function
  | Int i                 -> i
  | Bin_op (e1, Plus, e2) -> eval e1 + eval e2
  | Bin_op (e1, Mult, e2) -> eval e1 * eval e2

(* Test AST *)
let result (* true *) =
  let e1 = Bin_op (Int 123, Plus, Bin_op (Int 123, Mult, Int 234)) in
  let e2 = parse "123+123 * 234" in
  e1 = e2

(* Run the evaluator. *)
let exp_result = eval (parse "12+1*10") (* Should be 22. *)
