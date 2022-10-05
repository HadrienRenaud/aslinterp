(********************************************************************************************)
(* Operations *)

open Syntax
open Values
open Errors

let eval_binop v1 o v2 =
  match (v1, o, v2) with
  (* This should be working on all kinds of immutable values *)
  | _, Eq, _ -> Ok (VBool (v1 == v2))
  | _, NEq, _ -> Ok (VBool (v1 != v2))
  (* Operations on Int *)
  | VInt x, LT, VInt y -> Ok (VBool (x < y))
  | VInt x, Leq, VInt y -> Ok (VBool (x <= y))
  | VInt x, GT, VInt y -> Ok (VBool (x > y))
  | VInt x, Geq, VInt y -> Ok (VBool (x >= y))
  | VInt x, Plus, VInt y -> Ok (VInt Z.(x + y))
  | VInt x, Minus, VInt y -> Ok (VInt Z.(x - y))
  | VInt x, Mult, VInt y -> Ok (VInt Z.(x * y))
  | VInt _, DIV, VInt y when y = Z.zero -> Error DivisionByZero
  | VInt x, DIV, VInt y -> Ok (VInt Z.(x / y))
  | VInt _, MOD, VInt y when y = Z.zero -> Error DivisionByZero
  | VInt x, MOD, VInt y -> Ok (VInt Z.(x mod y))
  | VInt x, RSh, VInt y -> Ok (VInt Z.(x asr to_int y))
  | VInt x, LSh, VInt y -> Ok (VInt Z.(x lsl to_int y))
  | VInt x, Pow, VInt y -> Ok (VInt Z.(x ** to_int y))
  (* Operations on Real *)
  | VReal x, LT, VReal y -> Ok (VBool (x < y))
  | VReal x, Leq, VReal y -> Ok (VBool (x <= y))
  | VReal x, GT, VReal y -> Ok (VBool (x > y))
  | VReal x, Geq, VReal y -> Ok (VBool (x >= y))
  | VReal x, Plus, VReal y -> Ok (VReal Q.(x + y))
  | VReal x, Minus, VReal y -> Ok (VReal Q.(x - y))
  | VReal x, Mult, VReal y -> Ok (VReal Q.(x * y))
  | VReal _, RDiv, VReal y when y = Q.zero -> Error DivisionByZero
  | VReal x, RDiv, VReal y -> Ok (VReal Q.(x / y))
  | VReal x, Pow, VInt y ->
      Ok (VReal (Q.of_float (Q.to_float x ** Z.to_float y)))
  (* Operations on boolean *)
  | VBool a, BAnd, VBool b -> Ok (VBool (a && b))
  | VBool a, BOr, VBool b -> Ok (VBool (a || b))
  | VBool a, BEq, VBool b -> Ok (VBool (a == b))
  | VBool a, BImpl, VBool b -> Ok (VBool ((not a) || b))
  (* Operations on bitvectors *)
  (* Here we assume that length s1 == length s2 *)
  | VBitVec s1, EOR, VBitVec s2 -> Ok (VBitVec (List.map2 ( != ) s1 s2))
  | VBitVec s1, And, VBitVec s2 -> Ok (VBitVec (List.map2 ( && ) s1 s2))
  | VBitVec s1, Or, VBitVec s2 -> Ok (VBitVec (List.map2 ( || ) s1 s2))
  | VBitVec s1, Plus, VBitVec s2 ->
      let i1 = z_of_bitvector s1 in
      let i2 = z_of_bitvector s2 in
      let s3 = bitvector_of_z (Z.add i1 i2) (List.length s1) in
      Ok (VBitVec s3)
  | VBitVec s1, Minus, VBitVec s2 ->
      let i1 = z_of_bitvector s1 in
      let i2 = z_of_bitvector s2 in
      let s3 = bitvector_of_z (Z.sub i1 i2) (List.length s1) in
      Ok (VBitVec s3)
  | VBitVec s1, Plus, VInt i2 ->
      let i1 = z_of_bitvector s1 in
      let s3 = bitvector_of_z (Z.add i1 i2) (List.length s1) in
      Ok (VBitVec s3)
  | VBitVec s1, Minus, VInt i2 ->
      let i1 = z_of_bitvector s1 in
      let s3 = bitvector_of_z (Z.sub i1 i2) (List.length s1) in
      Ok (VBitVec s3)
  | _ ->
      Error
        (UnsupportedOperation
           (Format.asprintf "Unsupported operation %a for operands@ %a@ and %a"
              pp_print_binop o pp_print_value v1 pp_print_value v2))

let eval_unop o v =
  match (o, v) with
  | UBNeg, VBool b -> Ok (VBool (not b))
  | UNot, VBitVec s -> Ok (VBitVec (List.map not s))
  | UMinus, VInt x -> Ok (VInt (Z.neg x))
  | UMinus, VReal x -> Ok (VReal (Q.neg x))
  | _ ->
      Error
        (UnsupportedOperation
           (Format.asprintf "Unsupported operation %a for operand@ %a"
              pp_print_unop o pp_print_value v))

(********************************************************************************************)
(* Interpretor Functor *)

module type INTERPRETOR = sig
  type context

  val eval_expr :
    context -> Syntax.expr -> (context * Values.value) Errors.result

  val do_one_step_stmt :
    context -> Syntax.stmt -> (context * Syntax.stmt) Errors.result

  val eval_stmt : context -> Syntax.stmt -> context Errors.result
end

module MakeInterpretor (Ctx : Context.CONTEXT) = struct
  (********************************************************************************************)
  (* Expressions *)
  type context = Ctx.t

  open Syntax

  let ( let* ) = Result.bind

  let unpack_bool = function
    | VBool b -> Ok b
    | v ->
        Error
          (TypeError
             (Format.asprintf "Value %a is not a boolean." pp_print_value v))

  let rec eval_expr c e =
    match e with
    | ELiteral v -> Ok (c, v)
    (* Rule Extract-Context  *)
    | EVar x ->
        let* v = Ctx.find x c in
        Ok (c, v)
    (* Rule Reduce-Unop *)
    | EUnop (o, e') ->
        let* c', v = eval_expr c e' in
        let* v' = eval_unop o v in
        Ok (c', v')
    (* Rule Reduce-Binop *)
    | EBinop (e1, o, e2) ->
        let* c1, v1 = eval_expr c e1 in
        let* c2, v2 = eval_expr c1 e2 in
        let* v = eval_binop v1 o v2 in
        Ok (c2, v)
    | ECond (e1, e2, e3) ->
        let* c', v' = eval_expr c e1 in
        let* b = unpack_bool v' in
        let* c'', v'' = eval_expr c' (if b then e2 else e3) in
        Ok (c'', v'')

  (********************************************************************************************)
  (* Statements *)
  let rec do_one_step_stmt c s =
    match s with
    (* Rule Reduce-Then-Left *)
    | SThen (SPass, s2) -> Ok (c, s2)
    (* Rule Progress-Then-Left *)
    | SThen (s1, s2) ->
        let* c', s1' = do_one_step_stmt c s1 in
        Ok (c', SThen (s1', s2))
    (* Rule Reduce-Map-Write *)
    | SAssign (LEVar x, e) ->
        let* c', v = eval_expr c e in
        let* c'' = Ctx.set x v c' in
        Ok (c'', SPass)
    (* Rule Reduce-Cond *)
    | SCond (e, s1, s2) ->
        let* c', v = eval_expr c e in
        let* b = unpack_bool v in
        Ok (c', if b then s1 else s2)
    | SPass -> Error BlockedInterpretor

  let rec eval_stmt c s =
    match do_one_step_stmt c s with
    | Ok (c', SPass) -> Ok c'
    | Ok (c', s') -> eval_stmt c' s'
    | Error e -> Error e
end

module SequentialInterpretor =
  MakeInterpretor (Context.Logger (Context.SequentialContext))
