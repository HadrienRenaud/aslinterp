(* Prolog *)

module SMap = Map.Make (String)

type error =
  | DivisionByZero
  | UnsupportedOperation of string
  | SemanticError of string
  | UndefinedVariable of string
  | InterpretorError of string

let pp_print_error f e =
  match e with
  | DivisionByZero -> Format.pp_print_string f "Division by zero"
  | UnsupportedOperation s -> Format.fprintf f "Unsupported operation: %s" s
  | SemanticError s -> Format.fprintf f "Semantic error: %s" s
  | UndefinedVariable x -> Format.fprintf f "Variable %s is undefined." x
  | InterpretorError s -> Format.fprintf f "Internal error: %s" s

type 'a result = ('a, error) Result.t

(********************************************************************************************)
(* Operations *)

open Syntax
open Values

let eval_binop v1 o v2 =
  match (v1, o, v2) with
  | _, Eq, _ -> Ok (Bool (v1 == v2))
  | _, NEq, _ ->
      Ok (Bool (v1 != v2)) (* It should be working on all kinds of values *)
  (* Operations on Int *)
  | Int x, LT, Int y -> Ok (Bool (x < y))
  | Int x, Leq, Int y -> Ok (Bool (x <= y))
  | Int x, GT, Int y -> Ok (Bool (x > y))
  | Int x, Geq, Int y -> Ok (Bool (x >= y))
  | Int x, Plus, Int y -> Ok (Int (x + y))
  | Int x, Minus, Int y -> Ok (Int (x - y))
  | Int x, Mult, Int y -> Ok (Int (x * y))
  | Int _, DIV, Int 0 -> Error DivisionByZero
  | Int x, DIV, Int y -> Ok (Int (x / y))
  | Int _, MOD, Int 0 -> Error DivisionByZero
  | Int x, MOD, Int y -> Ok (Int (x mod y))
  | Int x, RSh, Int y -> Ok (Int (x asr y))
  | Int x, LSh, Int y -> Ok (Int (x lsl y))
  | Int x, Pow, Int y -> Ok (Int (int_pow x y))
  (* Operations on Real *)
  | Real x, LT, Real y -> Ok (Bool (x < y))
  | Real x, Leq, Real y -> Ok (Bool (x <= y))
  | Real x, GT, Real y -> Ok (Bool (x > y))
  | Real x, Geq, Real y -> Ok (Bool (x >= y))
  | Real x, Plus, Real y -> Ok (Real (x +. y))
  | Real x, Minus, Real y -> Ok (Real (x -. y))
  | Real x, Mult, Real y -> Ok (Real (x *. y))
  | Real _, RDiv, Real 0. -> Error DivisionByZero
  | Real x, RDiv, Real y -> Ok (Real (x /. y))
  | Real x, Pow, Int y -> Ok (Real (x ** float_of_int y))
  (* Operations on boolean *)
  | Bool a, BAnd, Bool b -> Ok (Bool (a && b))
  | Bool a, BOr, Bool b -> Ok (Bool (a || b))
  | Bool a, BEq, Bool b -> Ok (Bool (a == b))
  | Bool a, BImpl, Bool b -> Ok (Bool ((not a) || b))
  (* Operations on bitstrings *)
  (* Here we assume that length s1 == length s2 *)
  | Bitstr s1, EOR, Bitstr s2 -> Ok (Bitstr (Array.map2 ( != ) s1 s2))
  | Bitstr s1, And, Bitstr s2 -> Ok (Bitstr (Array.map2 ( && ) s1 s2))
  | Bitstr s1, Or, Bitstr s2 -> Ok (Bitstr (Array.map2 ( || ) s1 s2))
  | Bitstr s1, Plus, Bitstr s2 ->
      let i1 = int_of_bitstring s1 in
      let i2 = int_of_bitstring s2 in
      let s3 = bitstring_of_int (i1 + i2) (Array.length s1) in
      Ok (Bitstr s3)
  | Bitstr s1, Minus, Bitstr s2 ->
      let i1 = int_of_bitstring s1 in
      let i2 = int_of_bitstring s2 in
      let s3 = bitstring_of_int (i1 - i2) (Array.length s1) in
      Ok (Bitstr s3)
  | Bitstr s1, Plus, Int i2 ->
      let i1 = int_of_bitstring s1 in
      let s3 = bitstring_of_int (i1 + i2) (Array.length s1) in
      Ok (Bitstr s3)
  | Bitstr s1, Minus, Int i2 ->
      let i1 = int_of_bitstring s1 in
      let s3 = bitstring_of_int (i1 - i2) (Array.length s1) in
      Ok (Bitstr s3)
  | _ ->
      Error
        (UnsupportedOperation
           (Format.asprintf "Unsupported operation %a for operands@ %a@ and %a"
              pp_print_binop o pp_print_value v1 pp_print_value v2))

let eval_unop o v =
  match (o, v) with
  | UBNeg, Bool b -> Ok (Bool (not b))
  | UNot, Bitstr s -> Ok (Bitstr (Array.map not s))
  | UMinus, Int x -> Ok (Int ~-x)
  | UMinus, Real x -> Ok (Real ~-.x)
  | _ ->
      Error
        (UnsupportedOperation
           (Format.asprintf "Unsupported operation %a for operand@ %a"
              pp_print_unop o pp_print_value v))

(********************************************************************************************)
(* Contexts *)

type context = { v : Values.value SMap.t; s : Syntax.subpgm SMap.t }

let ctx_find_opt_var x c = SMap.find_opt x c.v

let ctx_find_res_var x c =
  ctx_find_opt_var x c |> Option.to_result ~none:(UndefinedVariable x)

let ctx_empty : context = { v = SMap.empty; s = SMap.empty }
let ctx_update_var x v c = { s = c.s; v = SMap.add x v c.v }

let pp_print_context f c =
  let pp_print_var f e =
    let x, v = e in
    Format.fprintf f "@[<2>\"%s\":@ %a@]" x pp_print_value v
  in
  Format.fprintf f "@[<hv 2>{ %a@] }"
    (Format.pp_print_seq
       ~pp_sep:(fun f () -> Format.fprintf f ",@ ")
       pp_print_var)
    (SMap.to_seq c.v)

(********************************************************************************************)
(* Expressions *)

open Syntax

let ( let* ) = Result.bind

let rec do_one_step_expr c e =
  match e with
  (* Do nothing on literals *)
  | ELiteral _ -> Error (InterpretorError "Trying to reduce a literal.")
  (* Rule Extract-Context  *)
  | EVar x ->
      let* v = ctx_find_res_var x c in
      Ok (c, ELiteral v)
  (* Rules Reduce-Unop-* *)
  | EUnop (o, ELiteral v) ->
      let* v' = eval_unop o v in
      Ok (c, ELiteral v')
  (* Rules Progress-Unop-* *)
  | EUnop (o, e') ->
      let* c, e'' = do_one_step_expr c e' in
      Ok (c, EUnop (o, e''))
  (* Rules Reduce-Binop-* *)
  | EBinop (ELiteral v1, o, ELiteral v2) ->
      let* v = eval_binop v1 o v2 in
      Ok (c, ELiteral v)
  (* Rules Progress-Binop-Right-* *)
  | EBinop (e1, o, e2) when not (is_literal e2) ->
      let* c, e2' = do_one_step_expr c e2 in
      Ok (c, EBinop (e1, o, e2'))
  (* Rules Progress-Binop-Left-* *)
  | EBinop (e1, o, e2) when not (is_literal e1) ->
      let* c, e1' = do_one_step_expr c e1 in
      Ok (c, EBinop (e1', o, e2))
  (* Final match to guard everything.
     Should not happen, but otherwise this does not compile. *)
  | _ ->
      Error
        (InterpretorError
           (Format.asprintf "Interpretor blocked on expression:@ %a"
              pp_print_expr e))

let rec eval_expr c e =
  match do_one_step_expr c e with
  | Ok (c', ELiteral v) -> Ok (c', v)
  | Ok (c', e') -> eval_expr c' e'
  | Error err -> Error err

(********************************************************************************************)
(* Statements *)

let rec do_one_step_stmt c s =
  match s with
  | SPass -> Error (InterpretorError "Blocked at a pass statement")
  (* Rule Reduce-Then-Left *)
  | SThen (SPass, s2) -> Ok (c, s2)
  (* Rule Progress-Then-Left *)
  | SThen (s1, s2) ->
      let* c', s1' = do_one_step_stmt c s1 in
      Ok (c', SThen (s1', s2))
  (* Rule Reduce-Assign *)
  | SAssign (LEVar x, ELiteral v) ->
      let c' = ctx_update_var x v c in
      Ok (c', SPass)
  (* Rule Progress-Assign *)
  | SAssign (LEVar x, e) ->
      let* c', e' = do_one_step_expr c e in
      Ok (c', SAssign (LEVar x, e'))

let rec eval_stmt c s =
  match do_one_step_stmt c s with
  | Ok (c', SPass) -> Ok c'
  | Ok (c', s') -> eval_stmt c' s'
  | Error e -> Error e
