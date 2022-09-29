(********************************************************************************************)
(* Operations *)

open Syntax
open Values
open Errors

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
  | Int x, Plus, Int y -> Ok (Int Z.(x + y))
  | Int x, Minus, Int y -> Ok (Int Z.(x - y))
  | Int x, Mult, Int y -> Ok (Int Z.(x * y))
  | Int _, DIV, Int y when y = Z.zero -> Error DivisionByZero
  | Int x, DIV, Int y -> Ok (Int Z.(x / y))
  | Int _, MOD, Int y when y = Z.zero -> Error DivisionByZero
  | Int x, MOD, Int y -> Ok (Int Z.(x mod y))
  | Int x, RSh, Int y -> Ok (Int Z.(x asr Z.to_int y))
  | Int x, LSh, Int y -> Ok (Int Z.(x lsl Z.to_int y))
  | Int x, Pow, Int y -> Ok (Int Z.(x ** Z.to_int y))
  (* Operations on Real *)
  | Real x, LT, Real y -> Ok (Bool (x < y))
  | Real x, Leq, Real y -> Ok (Bool (x <= y))
  | Real x, GT, Real y -> Ok (Bool (x > y))
  | Real x, Geq, Real y -> Ok (Bool (x >= y))
  | Real x, Plus, Real y -> Ok (Real Q.(x + y))
  | Real x, Minus, Real y -> Ok (Real Q.(x - y))
  | Real x, Mult, Real y -> Ok (Real Q.(x * y))
  | Real _, RDiv, Real y when y = Q.zero -> Error DivisionByZero
  | Real x, RDiv, Real y -> Ok (Real Q.(x / y))
  | Real x, Pow, Int y -> Ok (Real (Q.of_float (Q.to_float x ** Z.to_float y)))
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
      let i1 = z_of_bitstring s1 in
      let i2 = z_of_bitstring s2 in
      let s3 = bitstring_of_z (Z.add i1 i2) (Array.length s1) in
      Ok (Bitstr s3)
  | Bitstr s1, Minus, Bitstr s2 ->
      let i1 = z_of_bitstring s1 in
      let i2 = z_of_bitstring s2 in
      let s3 = bitstring_of_z (Z.sub i1 i2) (Array.length s1) in
      Ok (Bitstr s3)
  | Bitstr s1, Plus, Int i2 ->
      let i1 = z_of_bitstring s1 in
      let s3 = bitstring_of_z (Z.add i1 i2) (Array.length s1) in
      Ok (Bitstr s3)
  | Bitstr s1, Minus, Int i2 ->
      let i1 = z_of_bitstring s1 in
      let s3 = bitstring_of_z (Z.sub i1 i2) (Array.length s1) in
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
  | UMinus, Int x -> Ok (Int (Z.neg x))
  | UMinus, Real x -> Ok (Real (Q.neg x))
  | _ ->
      Error
        (UnsupportedOperation
           (Format.asprintf "Unsupported operation %a for operand@ %a"
              pp_print_unop o pp_print_value v))

(********************************************************************************************)
(* Interpretor Functor *)

module type INTERPRETOR = sig
  type context

  val do_one_step_expr :
    context -> Syntax.expr -> (context * Syntax.expr) Errors.result

  val eval_expr :
    context -> Syntax.expr -> (context * Values.value) Errors.result

  val do_one_step_stmt :
    context -> Syntax.stmt -> (context * Syntax.stmt) Errors.result

  val eval_stmt : context -> Syntax.stmt -> context Errors.result
end

module type SEMANTIC_DESCRIPTOR = sig
  val semi_column_concurrent : bool
end

module SequentialSemantics = struct
  let semi_column_concurrent = false
end

module MakeInterpretor (Ctx : Context.CONTEXT) (SD : SEMANTIC_DESCRIPTOR) =
struct
  (********************************************************************************************)
  (* Expressions *)
  type context = Ctx.t

  open Syntax

  let ( let* ) = Result.bind

  let value_to_index v =
    match v with
    | Int z -> Ok (IInt z)
    | String s -> Ok (IString s)
    | _ ->
        Error
          (UnsupportedOperation
             (Format.asprintf "Cannot index by value %a." pp_print_value v))

  let unpack_map = function
    | Map a -> Ok a
    | v ->
        Error
          (UnsupportedOperation
             (Format.asprintf "Cannot index %a" pp_print_value v))

  let rec do_one_step_expr c e =
    match e with
    (* Rule Extract-Context  *)
    | EVar x ->
        let* v = Ctx.find x c in
        Ok (c, ELiteral v)
    (* Rule Reduce-Unop *)
    | EUnop (o, ELiteral v) ->
        let* v' = eval_unop o v in
        Ok (c, ELiteral v')
    (* Rule Reduce-Binop *)
    | EBinop (ELiteral v1, o, ELiteral v2) ->
        let* v = eval_binop v1 o v2 in
        Ok (c, ELiteral v)
    (* Rule Reduce-Map-Access *)
    | EMapAccess (ELiteral (Map l), ELiteral v) -> (
        let* i = value_to_index v in
        match List.assoc_opt i l with
        | Some v' -> Ok (c, ELiteral v')
        | None ->
            Error
              (IndexOutOfBounds
                 (Format.asprintf "Index %a not defined in %a" pp_print_index i
                    pp_print_value (Map l))))
    (*******************)
    (* Rule Progress-Unop *)
    | EUnop (o, e') ->
        let* c, e'' = do_one_step_expr c e' in
        Ok (c, EUnop (o, e''))
    (* Rule Progress-Binop-Right *)
    | EBinop (e1, o, e2) when not (is_literal e2) ->
        let* c, e2' = do_one_step_expr c e2 in
        Ok (c, EBinop (e1, o, e2'))
    (* Rule Progress-Binop-Left *)
    | EBinop (e1, o, e2) when not (is_literal e1) ->
        let* c, e1' = do_one_step_expr c e1 in
        Ok (c, EBinop (e1', o, e2))
    (* Rule Progress-Map-Access-left *)
    | EMapAccess (e1, e2) when not (is_literal e1) ->
        let* c, e1' = do_one_step_expr c e1 in
        Ok (c, EMapAccess (e1', e2))
    (* Rule Progress-Map-Access-Right *)
    | EMapAccess (e1, e2) when not (is_literal e2) ->
        let* c, e2' = do_one_step_expr c e2 in
        Ok (c, EMapAccess (e1, e2'))
    (* Final match to guard everything.
       Should not happen, but otherwise this does not compile. *)
    | _ -> Error BlockedInterpretor

  let rec eval_expr c e =
    match do_one_step_expr c e with
    | Ok (c', ELiteral v) -> Ok (c', v)
    | Ok (c', e') -> eval_expr c' e'
    | Error err -> Error err

  (********************************************************************************************)
  (* Statements *)

  let rec do_one_step_stmt c s =
    match s with
    (* Rule Reduce-Then-Left *)
    | SThen (SPass, s2) -> Ok (c, s2)
    (* Rule Progress-Then-Left *)
    | SThen (s1, s2) ->
        if SD.semi_column_concurrent then raise (Failure "Not yet implemented")
        else
          let* c', s1' = do_one_step_stmt c s1 in
          Ok (c', SThen (s1', s2))
    (* Rule Reduce-Assign *)
    | SAssign (LEVar x, ELiteral v) ->
        let* c' = Ctx.set x v c in
        Ok (c', SPass)
    (* Rule Reduce-Map-Write *)
    | SAssign (LEMapWrite (x, ELiteral v1), ELiteral v2) ->
        let* i = value_to_index v1 in
        let* ma = Ctx.find x c in
        let* a = unpack_map ma in
        let a' = (i, v2) :: List.remove_assoc i a in
        let* c' = Ctx.set x (Map a') c in
        Ok (c', SPass)
    (* Rule Progress-Assign *)
    | SAssign (x, e) when not (is_literal e) ->
        let* c', e' = do_one_step_expr c e in
        Ok (c', SAssign (x, e'))
    (* Rule Reduce-Cond *)
    | SCond (ELiteral (Bool b), s1, s2) -> Ok (c, if b then s1 else s2)
    (* Rule Progress-Cond *)
    | SCond (e, s1, s2) ->
        let* c', e' = do_one_step_expr c e in
        Ok (c', SCond (e', s1, s2))
    | _ -> Error BlockedInterpretor

  let rec eval_stmt c s =
    match do_one_step_stmt c s with
    | Ok (c', SPass) -> Ok c'
    | Ok (c', s') -> eval_stmt c' s'
    | Error e -> Error e
end

module SequentialInterpretor =
  MakeInterpretor
    (Context.Logger (Context.SequentialContext)) (SequentialSemantics)
